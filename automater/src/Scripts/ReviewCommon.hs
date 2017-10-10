{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.ReviewCommon where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action.Reified
import Scripts.Common
import Control.Retry
import qualified Streaming.Prelude as S
import qualified Data.Foldable as F

data ReviewBaseConf = ReviewBaseConf
  { reviewType :: ReviewType
  , reviewerType :: ReviewerType
  , fundingYear :: FundingYear
  } deriving Show

adminInitial2017 :: ReviewBaseConf
adminInitial2017 = ReviewBaseConf RevAdminCorrection RevInitial FY2017

adminFinal2017 :: ReviewBaseConf
adminFinal2017 = ReviewBaseConf RevAdminCorrection RevFinal FY2017

adminSolix2017 :: ReviewBaseConf
adminSolix2017 = ReviewBaseConf RevAdminCorrection RevSolix FY2017

adminUsac2017 :: ReviewBaseConf
adminUsac2017 = ReviewBaseConf RevAdminCorrection RevUsac FY2017

appealInitial2017 :: ReviewBaseConf
appealInitial2017 = ReviewBaseConf RevAppeals RevInitial FY2017

appealFinal2017 :: ReviewBaseConf
appealFinal2017 = ReviewBaseConf RevAppeals RevFinal FY2017

appealSolix2017 :: ReviewBaseConf
appealSolix2017 = ReviewBaseConf RevAppeals RevSolix FY2017

appealUsac2017 :: ReviewBaseConf
appealUsac2017 = ReviewBaseConf RevAppeals RevUsac FY2017

spinInitial2017 :: ReviewBaseConf
spinInitial2017 = ReviewBaseConf RevSpinChange RevInitial FY2017

spinFinal2017 :: ReviewBaseConf
spinFinal2017 = ReviewBaseConf RevSpinChange RevFinal FY2017

spinSolix2017 :: ReviewBaseConf
spinSolix2017 = ReviewBaseConf RevSpinChange RevSolix FY2017

spinUsac2017 :: ReviewBaseConf
spinUsac2017 = ReviewBaseConf RevSpinChange RevUsac FY2017

form486Initial2017 :: ReviewBaseConf
form486Initial2017 = ReviewBaseConf RevForm486 RevInitial FY2017

form486Final2017 :: ReviewBaseConf
form486Final2017 = ReviewBaseConf RevForm486 RevFinal FY2017

form486Solix2017 :: ReviewBaseConf
form486Solix2017 = ReviewBaseConf RevForm486 RevSolix FY2017

form486Usac2017 :: ReviewBaseConf
form486Usac2017 = ReviewBaseConf RevForm486 RevUsac FY2017

servSubInitial2017 :: ReviewBaseConf
servSubInitial2017 = ReviewBaseConf RevServSub RevInitial FY2017

servSubFinal2017 :: ReviewBaseConf
servSubFinal2017 = ReviewBaseConf RevServSub RevFinal FY2017

servSubSolix2017 :: ReviewBaseConf
servSubSolix2017 = ReviewBaseConf RevServSub RevSolix FY2017

servSubUsac2017 :: ReviewBaseConf
servSubUsac2017 = ReviewBaseConf RevServSub RevUsac FY2017

form500Initial2017 :: ReviewBaseConf
form500Initial2017 = ReviewBaseConf RevForm500 RevInitial FY2017

form500Final2017 :: ReviewBaseConf
form500Final2017 = ReviewBaseConf RevForm500 RevFinal FY2017

form500Solix2017 :: ReviewBaseConf
form500Solix2017 = ReviewBaseConf RevForm500 RevSolix FY2017

form500Usac2017 :: ReviewBaseConf
form500Usac2017 = ReviewBaseConf RevForm500 RevUsac FY2017

data ReviewType
  = RevSelect
  | RevAppeals
  | RevForm486
  | RevCOMAD
  | RevForm500
  | RevSpinChange
  | RevServSub
  | RevAdminCorrection
  | RevInvalid Text
  deriving (Show, Eq, Read)

instance IsString ReviewType where
  fromString "-- Select a Review Type --" = RevSelect
  fromString "Appeals" = RevAppeals
  fromString "FCC Form 486/CIPA" = RevForm486
  fromString "COMAD" = RevCOMAD
  fromString "FCC Form 500" = RevForm500
  fromString "SPIN Change" = RevSpinChange
  fromString "Service Substitution" = RevServSub
  fromString "Administrative Correction" = RevAdminCorrection
  fromString s = RevInvalid $ pack s

data ReviewerType
  = ReviewerSelect
  | RevInitial
  | RevFinal
  | RevSolix
  | RevUsac
  | RevHSInit
  | RevHSFinal
  | ReviewerInvalid Text
  deriving (Show, Eq, Read)

instance IsString ReviewerType where
  fromString "-- Select a Reviewer Type --" = ReviewerSelect
  fromString "Initial Review" = RevInitial
  fromString "Final Review" = RevFinal
  fromString "Solix QA Review" = RevSolix
  fromString "USAC QA Review" = RevUsac
  fromString "Heightened Scrutiny Initial Review" = RevHSInit
  fromString "Heightened Scrutiny Final Review" = RevHSFinal
  fromString s = ReviewerInvalid $ pack s

data FundingYear
  = FYSelect
  | FY2016
  | FY2017
  | FYInvalid Text
  deriving (Show, Eq, Read)

instance IsString FundingYear where
  fromString "-- Select a Funding Year --" = FYSelect
  fromString "2016" = FY2016
  fromString "2017" = FY2017
  fromString s = FYInvalid $ pack s

dropdownIndex :: (IsString s, Eq s) => s -> DropdownField -> Maybe Int
dropdownIndex s df = df ^? dfChoices . itraversed . to (fromString . unpack) . ifiltered filter . withIndex . _1 . to (+1)
  where
    filter _ v = v == s

dropdownSelect :: (IsString s, Eq s) => s -> DropdownField -> Maybe DropdownField
dropdownSelect s df = do
  idx <- dropdownIndex s df
  return $ dfValue .~ idx $ df

dropdownUpdate' :: (AsJSON t, AsValue t, Plated t, IsString s, Eq s, Contravariant f, Applicative f)
                => Text -> s -> Over (->) f t t (Either Text Update) (Either Text Update)
dropdownUpdate' label s = failing mkUpdate err
  where
    fetchSet = getDropdown label . to (dropdownSelect s) . traverse
    mkUpdate = fetchSet . to toUpdate . to Right
    err = to $ const $ Left "Could not set dropdown!"

dropdownUpdateF' :: (Eq t, IsString t, Plated s, AsValue s, AsJSON s) =>
                    Text -> t -> ReifiedMonadicFold m s (Either Text Update)
dropdownUpdateF' label s = MonadicFold $ dropdownUpdate' label s

data ReviewConf = ReviewConf
  { revTaskVar :: TVar DistributeTask
  , revChan :: TChan (ThreadControl RecordRef)
  }

newReviewConf :: MonadIO m => m ReviewConf
newReviewConf = ReviewConf <$> atomically (newTVar Produce) <*> atomically newTChan

delaySecs :: MonadBase IO m => Int -> m ()
delaySecs n = threadDelay $ n * 1000000

retryIt :: MonadIO m => m (Either SomeException a) -> m (Either SomeException a)
retryIt act = retrying reviewRetryPolicy shouldRetry (const act)
  where
    shouldRetry status (Left _) = do
      putStrLn $ "Retrying with a " <> tshow (rsCumulativeDelay status) <> " delay"
      pure True
    shouldRetry _ (Right _) = pure False

reviewRetryPolicy :: Monad m => RetryPolicyM m
reviewRetryPolicy = exponentialBackoff 1000000 `mappend` limitRetries 3

distributeLinks :: Text -> ReportId -> ReviewConf -> Value -> Appian Value
distributeLinks actionName rid conf v = do
  gf <- handleMissing "FRN Case Grid" v $ v ^? getGridFieldCell . traverse
  let execReview = viewRelatedActions v >=> uncurry (executeRelatedAction actionName)
  mVal <- distributeTasks (revTaskVar conf) (revChan conf) (getAllLinks rid v) execReview
  handleMissing "Could not select FRN!" v mVal

getAllLinks :: ReportId -> Value -> S.Stream (S.Of (ThreadControl RecordRef)) Appian ()
getAllLinks rid v = do
  mIdents <- lift $ foldGridFieldPagesReport rid (MonadicFold $ getGridFieldCell . traverse) (accumLinks v) (Just mempty) v
  case mIdents of
    Nothing -> throwM $ MissingComponentException ("There are no identifiers!", v)
    Just idents -> S.each $ fmap Item idents <> pure Finished

accumLinks :: Value -> Maybe (Vector RecordRef) -> GridField GridFieldCell -> Appian (Maybe (Vector RecordRef), Value)
accumLinks val l gf = return (l', val)
  where
    l' = (<>) <$> l <*> (gf ^? gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2)

selectCheckbox :: GridFieldIdent -> GridField a -> GridField a
selectCheckbox ident = gfSelection . traverse . _Selectable . gslSelected .~ [ident]

foldGridField' :: (b -> GridFieldIdent -> Appian b) -> b -> GridField a -> Appian b
foldGridField' f b gf = do
  let boxes = gf ^.. gfIdentifiers . traverse . traverse
  F.foldlM f b boxes
