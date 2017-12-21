{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Scripts.Test
import Control.Retry
import qualified Streaming.Prelude as S
import qualified Data.Foldable as F
import Control.Monad.Time
import qualified Data.Csv as Csv
import Data.Random (MonadRandom)
import Control.Monad.Except

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

spinInitial2016 :: ReviewBaseConf
spinInitial2016 = ReviewBaseConf RevSpinChange RevInitial FY2016

spinFinal2016 :: ReviewBaseConf
spinFinal2016 = ReviewBaseConf RevSpinChange RevFinal FY2016

spinSolix2016 :: ReviewBaseConf
spinSolix2016 = ReviewBaseConf RevSpinChange RevSolix FY2016

spinUsac2016 :: ReviewBaseConf
spinUsac2016 = ReviewBaseConf RevSpinChange RevUsac FY2016

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

form500Initial2016 :: ReviewBaseConf
form500Initial2016 = ReviewBaseConf RevForm500 RevInitial FY2016

form500Final2016 :: ReviewBaseConf
form500Final2016 = ReviewBaseConf RevForm500 RevFinal FY2016

form500Solix2016 :: ReviewBaseConf
form500Solix2016 = ReviewBaseConf RevForm500 RevSolix FY2016

form500Usac2016 :: ReviewBaseConf
form500Usac2016 = ReviewBaseConf RevForm500 RevUsac FY2016

comadInitial2016 :: ReviewBaseConf
comadInitial2016 = ReviewBaseConf RevCOMAD RevInitial FY2016

comadFinal2016 :: ReviewBaseConf
comadFinal2016 = ReviewBaseConf RevCOMAD RevFinal FY2016

comadSolix2016 :: ReviewBaseConf
comadSolix2016 = ReviewBaseConf RevCOMAD RevSolix FY2016

comadUsac2016 :: ReviewBaseConf
comadUsac2016 = ReviewBaseConf RevCOMAD RevUsac FY2016

comadInitial2017 :: ReviewBaseConf
comadInitial2017 = ReviewBaseConf RevCOMAD RevInitial FY2017

comadFinal2017 :: ReviewBaseConf
comadFinal2017 = ReviewBaseConf RevCOMAD RevFinal FY2017

comadSolix2017 :: ReviewBaseConf
comadSolix2017 = ReviewBaseConf RevCOMAD RevSolix FY2017

comadUsac2017 :: ReviewBaseConf
comadUsac2017 = ReviewBaseConf RevCOMAD RevUsac FY2017

data ReviewType
  = RevSelect
  | RevAppeals
  | RevForm486
  | RevCOMAD
  | RevForm500
  | RevSpinChange
  | RevServSub
  | RevAdminCorrection
  | RevSRCSpinChange
  | RevBulkSpinChange
  deriving (Show, Eq, Read)

instance Parseable ReviewType where
  parseElement "-- Select a Review Type --" = pure RevSelect
  parseElement "Appeals" = pure RevAppeals
  parseElement "FCC Form 486/CIPA" = pure RevForm486
  parseElement "COMAD" = pure RevCOMAD
  parseElement "FCC Form 500" = pure RevForm500
  parseElement "SPIN Change" = pure RevSpinChange
  parseElement "Service Substitution" = pure RevServSub
  parseElement "Administrative Correction" = pure RevAdminCorrection
  parseElement "Bulk SPIN" = pure RevBulkSpinChange
  parseElement "SRC SPIN" = pure RevSRCSpinChange
  parseElement s = throwM $ ParseException $ tshow s <> " is not a recognized Review Type."

data ReviewerType
  = ReviewerSelect
  | RevInitial
  | RevFinal
  | RevSolix
  | RevUsac
  | RevHSInit
  | RevHSFinal
  deriving (Show, Eq, Read)

instance Parseable ReviewerType where
  parseElement "-- Select a Reviewer Type --" = pure ReviewerSelect
  parseElement "Initial Review" = pure RevInitial
  parseElement "Final Review" = pure RevFinal
  parseElement "Solix QA Review" = pure RevSolix
  parseElement "USAC QA Review" = pure RevUsac
  parseElement "Heightened Scrutiny Initial Review" = pure RevHSInit
  parseElement "Heightened Scrutiny Final Review" = pure RevHSFinal
  parseElement s = throwM $ ParseException $ tshow s <> " is not a recognized Reviewer Type."

data ReviewConf = ReviewConf
  { revTaskVar :: TVar DistributeTask
  , revChan :: TChan (ThreadControl RecordRef)
  }

newReviewConf :: MonadIO m => m ReviewConf
newReviewConf = ReviewConf <$> atomically (newTVar Produce) <*> atomically newTChan

-- delaySecs :: (MonadDelay m, MonadThreadId m) => Int -> m ()
-- delaySecs n = threadDelay $ n * 1000000

retryIt :: MonadIO m => m (Either SomeException a) -> m (Either SomeException a)
retryIt act = retrying reviewRetryPolicy shouldRetry (const act)
  where
    shouldRetry status (Left _) = do
      putStrLn $ "Retrying with a " <> tshow (rsCumulativeDelay status) <> " delay"
      pure True
    shouldRetry _ (Right _) = pure False

reviewRetryPolicy :: Monad m => RetryPolicyM m
reviewRetryPolicy = exponentialBackoff 1000000 `mappend` limitRetries 1

distributeLinks_ :: (MonadThrow m, RunClient m, MonadTime m, MonadLogger m, MonadCatch m, MonadIO m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => (RecordRef -> AppianT m Value) -> ReportId -> ReviewConf -> Value -> AppianT m Value
distributeLinks_ action rid conf v = do
  gf <- handleMissing "FRN Case Grid" v $ v ^? getGridFieldCell . traverse
  mVal <- distributeTasks (revTaskVar conf) (revChan conf) (getAllLinks rid v) action
  handleMissing "Could not select FRN!" v mVal

distributeLinks :: (RunClient m, MonadThrow m, MonadTime m, MonadLogger m, MonadCatch m, MonadIO m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => Text -> ReportId -> ReviewConf -> Value -> AppianT m Value
distributeLinks actionName rid conf v = distributeLinks_ (viewRelatedActions v >=> uncurry (executeRelatedAction actionName)) rid conf v

getAllLinks :: (RunClient m, MonadLogger m, MonadThrow m, MonadTime m, MonadCatch m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => ReportId -> Value -> S.Stream (S.Of (ThreadControl RecordRef)) (AppianT m) ()
getAllLinks rid v = do
  mIdents <- lift $ foldGridFieldPagesReport rid (MonadicFold $ getGridFieldCell . traverse) (accumLinks v) (Just mempty) v
  case mIdents of
    Nothing -> throwError $ MissingComponentError ("There are no identifiers!", v)
    Just idents -> S.each $ fmap Item idents <> pure Finished

accumLinks :: Monad m => Value -> Maybe (Vector RecordRef) -> GridField GridFieldCell -> AppianT m (Maybe (Vector RecordRef), Value)
accumLinks val l gf = return (l', val)
  where
    l' = (<>) <$> l <*> (gf ^? gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2)

addNotes :: (RunClient m, MonadLogger m, MonadTime m, MonadThrow m, MonadCatch m, MonadGen m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => Value -> AppianT m Value
addNotes val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) makeNotes val val

makeNotes :: (RunClient m, MonadThrow m, MonadTime m, MonadLogger m, MonadCatch m, MonadGen m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
makeNotes val gf = do
  v <- foldGridField' makeNote val gf
  return (v, v)

makeNote :: (MonadThrow m, RunClient m, MonadTime m, MonadGen m, MonadLogger m, MonadCatch m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => Value -> GridFieldIdent -> AppianT m Value
makeNote val ident = do
  gf <- handleMissing "FRN Note Grid" val $ val ^? getGridFieldCell . traverse
  val' <- sendUpdates "Notes: Select FRN Checkbox" (MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))
                                           ) val
  case val' ^? getButton "Edit Note" of
    Nothing ->
          sendUpdates "Click Add Note" (MonadicFold (to $ buttonUpdate "Add Note")) val'
      >>= sendUpdates "Enter Note Text" (paragraphArbitraryUpdate "Note Text" 10000
                                      <|> MonadicFold (to $ buttonUpdate "Submit New Note")
                                      )
    Just _ -> sendUpdates "Edit Note" (MonadicFold (to $ buttonUpdate "Edit Note")) val'
      >>= sendUpdates "Enter Note Text" (paragraphArbitraryUpdate "Note Text" 10000
                                         <|> MonadicFold (to $ buttonUpdate "Submit Note Change")
                                        )

newtype CaseNumber = CaseNumber
  { _caseNumber :: Int
  } deriving (Show, Eq, Num, Csv.FromField)

makeLenses ''CaseNumber

data ReviewConf' = ReviewConf'
  { _frnCaseNumber :: CaseNumber
  , _reviewer :: Login
  } deriving (Show, Eq)

makeLenses ''ReviewConf'

instance Csv.FromNamedRecord ReviewConf' where
  parseNamedRecord r = ReviewConf'
    <$> r Csv..: "Case Id"
    <*> Csv.parseNamedRecord r

instance HasLogin ReviewConf' where
  getLogin conf = conf ^. reviewer

  -- This needs to be renamed to replace the myAssignedReport function below.
myAssignedReport :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => Maybe CaseNumber -> ReviewBaseConf -> AppianT m (ReportId, Value)
myAssignedReport mCaseNum conf = do
  (rid, v) <- openReport "My Assigned Post-Commit Assignments"
  let filterCase v = case mCaseNum of
        Nothing -> return v
        Just caseNum -> sendReportUpdates rid "Application/Request Number" (MonadicFold $ to $ textUpdate "Application/Request Number" (caseNum ^. caseNumber . to tshow)) v
  res <- editReport rid
    >>= sendReportUpdates rid "Select Review Type" (dropdownUpdateF' "Review Type" (reviewType conf))
    >>= sendReportUpdates rid "Select Reviewer Type" (dropdownUpdateF' "Reviewer Type" (reviewerType conf))
    >>= sendReportUpdates rid "Select Funding Year" (dropdownUpdateF' "Funding Year" (fundingYear conf))
    >>= filterCase
    >>= sendReportUpdates rid "Click Apply Filters" (MonadicFold (to (buttonUpdate "Apply Filters")))
--    >>= sendReportUpdates rid "Sort by Age" (MonadicFold $ getGridFieldCell . traverse . to setAgeSort . to toUpdate . to Right)
  return (rid, res)

--     -- Needs to be replaced by myAssignedReportTemp above
-- myAssignedReport :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadDelay m, MonadThreadId m, MonadRandom m, MonadError ServantError m) => ReviewBaseConf -> AppianT m (ReportId, Value)
-- myAssignedReport = myAssignedReportTemp Nothing
