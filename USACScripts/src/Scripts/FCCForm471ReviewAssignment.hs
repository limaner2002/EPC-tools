{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Scripts.FCCForm471ReviewAssignment where

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
import Appian.Internal.Arbitrary
import Control.Monad.Time
import Scripts.ReviewCommon
import Scripts.FCCForm471Common
import qualified Data.Csv as Csv
import Data.Random (MonadRandom)
import Control.Monad.Except
import Appian.Internal.Updates
import Scripts.Execute

data PIAReviewerType
  = PIAInitial
  | PIAFinal
  | PIAHSInitial
  | PIAQA
  | PIAHSFinal
  | PIAQAHS
  | PIAUsacQA
  | PIAUsacHS
  deriving (Show, Eq, Read)

instance Parseable PIAReviewerType where
  parseElement "PIA Initial Reviewer" = pure PIAInitial
  parseElement "Heightened Scrutiny Initial Reviewer" = pure PIAHSInitial
  parseElement "PIA Final Reviewer" = pure PIAFinal
  parseElement "QA Reviewer" = pure PIAQA
  parseElement "Heightened Scrutiny Final Reviewer" = pure PIAHSFinal
  parseElement "QA Heightened Scrutiny" = pure PIAQAHS
  parseElement "USAC QA Reviewer" = pure PIAUsacQA
  parseElement "USAC Heightened Scrutiny Reviewer" = pure PIAUsacHS
  parseElement str = Left $ tshow str <> " is not a recognized PIA Reviewer Type." -- throwM $ ParseException $ tshow str <> " is not a recognized PIA Reviewer Type."

instance Csv.FromField PIAReviewerType where
  parseField bs =
    case readMay (decodeUtf8 bs) of
      Nothing -> fail $ "Could not create PIAReviewerType from " <> show bs
      Just v -> return v

newtype BEN = BEN Int
  deriving (Show, Eq, Num)

instance Csv.FromField BEN where
  parseField bs = BEN <$> Csv.parseField bs

instance Csv.FromField FundingYear where
  parseField bs = do
    case parseElement $ decodeUtf8 bs of
      Left msg -> fail $ show msg
      Right val -> return val

data Form471ReviewConf = Form471ReviewConf
  { _confFormNum :: Form471Num
  , _confBen :: BEN
  , _confFY :: FundingYear
  , _confReviewType :: PIAReviewerType
  , _confReviewer :: Login
  } deriving Show

makeLenses ''Form471ReviewConf

instance Csv.FromNamedRecord Form471ReviewConf where
  parseNamedRecord r = Form471ReviewConf
    <$> r Csv..: "formNum"
    <*> r Csv..: "ben"
    <*> r Csv..: "fy"
    <*> r Csv..: "revType"
    <*> Csv.parseNamedRecord r

instance HasLogin Form471ReviewConf where
  getLogin conf = conf ^. confReviewer

benToText :: BEN -> Text
benToText (BEN n) = tshow n

form471Assign :: (RapidFire m, MonadGen m) => Form471ReviewConf -> AppianT m Value
form471Assign conf = do
  runReportByName "471 Reviews Assignment " $ assignmentReport conf
  use appianValue
  
assignmentReport :: Form471ReviewConf -> Report ()
assignmentReport conf = do
  let un = Identifiers [conf ^. confReviewer . username]
  
  update "Select Funding Year" (dropdownUpdateF' "Fund Year" (conf ^. confFY))
  update "Select 'Reviewer Type'" (dropdownUpdateF' "Reviewer Type" (conf ^. confReviewType))
  update "Select 'BEN'" (textUpdateF "BEN" $ benToText $ conf ^. confBen)
  update "Select Application" (MonadicFold $ getGridFieldCell . traverse . to (gridSelection [1]) . to toUpdate . to Right)
  update "Select Reviewer" (pickerUpdateF "Select a Reviewer" un)
  update "click 'Assign BEN(s) to Reviewer' button" (buttonUpdateF "Assign BEN(s) to Reviewer")
                                                           
runForm471Assign = runScriptExhaustive form471Assign

-- form471Assign :: (RapidFire m, MonadGen m) => Form471ReviewConf -> AppianT m Value
-- form471Assign conf = do
--   let un = Identifiers [conf ^. confReviewer . username]
--   (rid, v) <- openReport "471 Reviews Assignment "
--   editReport rid
--     >>= sendReportUpdates rid "Select Funding Year" (dropdownUpdateF' "Fund Year" (conf ^. confFY))
--     >>= sendReportUpdates rid "Enter BEN && Apply Filters" (dropdownUpdateF' "Reviewer Type" (conf ^. confReviewType)
--                                                              <|> MonadicFold (to $ textUpdate "BEN" $ benToText $ conf ^. confBen)
--                                                              <|> MonadicFold (to $ buttonUpdate "Apply Filters")
--                                                            )
--     >>= sendReportUpdates rid "Sort by Application Number" (MonadicFold $ getGridFieldCell . traverse . to setAppNumSort . to toUpdate . to Right)
--     >>= sendReportUpdates rid "Select Application" (MonadicFold $ getGridFieldCell . traverse . to (gridSelection [1]) . to toUpdate . to Right)
--     >>= sendReportUpdates rid "Assign Reviewer" (MonadicFold (to $ pickerUpdate "Select a Reviewer" un)
--                                                  <|> MonadicFold (to (buttonUpdate "Assign Application(s) to Reviewer"))
--                                                 )

setAppNumSort :: GridField a -> GridField a
setAppNumSort = gfSelection . traverse . failing _NonSelectable (_Selectable . gslPagingInfo) . pgISort .~ Just [SortField "applicationNumber" (Just True)]

gridSelection :: [Int] -> GridField a -> GridField a
gridSelection idxs gf = gfSelection . traverse . _Selectable . gslSelected .~ idents $ gf
  where
    idents = gf ^.. gfIdentifiers . traverse . itraversed . withIndex . filtered isDesired . _2
    isDesired (i, _) = i `elem` idxs

form471NumToText :: Form471Num -> Text
form471NumToText (Form471Num n) = tshow n

form471Review :: (RapidFire m, MonadGen m) => Form471ReviewConf -> AppianT m Value
form471Review conf = do
  -- (rid, v) <- openReport "My Assigned 471 Applications"
  -- editReport rid
  --   >>= sendReportUpdates rid "Select Funding Year" (dropdownUpdateF' "Fund Year" (conf ^. confFY))
  --   >>= sendReportUpdates rid "Enter Application Number & Apply Filters" (MonadicFold (to $ textUpdate "Application Number" (form471NumToText $ conf ^. confFormNum))
  --                                                                        <|> MonadicFold (to $ buttonUpdate "Apply Filters")
  --                                                                        )
  --   >>= clickApplication
  myAssigned471AppReport conf
  clickApplication "Manage Exceptions"
  v <- use appianValue

  clearAllExceptions v
    >>= addDecision (conf ^. confReviewType)
    >>= sendUpdates "Complete Review Step" (MonadicFold $ getButtonWith (\l -> l == "Complete PIA Review" || l == "Complete HS Review") . to toUpdate . to Right)

myAssigned471AppReport :: RapidFire m => Form471ReviewConf -> AppianT m ()
myAssigned471AppReport conf = do
  (rid, v) <- openReport "My Assigned 471 Applications"
  newVal <- editReport rid
    >>= sendReportUpdates rid "Select Funding Year" (dropdownUpdateF' "Fund Year" (conf ^. confFY))
    >>= sendReportUpdates rid "Enter Application Number & Apply Filters" (MonadicFold (to $ textUpdate "Application Number" (form471NumToText $ conf ^. confFormNum))
  
                                                                         <|> MonadicFold (to $ buttonUpdate "Apply Filters")
                                                                         )
  --   >>= clickApplication
  assign appianValue newVal

clickApplication :: RapidFire m => Text -> AppianT m ()
clickApplication actionName = do
  val <- use appianValue
  rref <- handleMissing "Record ref" val $ val ^? dropping 1 getGridFieldCell . traverse . gfColumns . at "Application Number" . traverse . _TextCellLink  . _2 . traverse
  (_, v) <- viewRelatedActions val rref
  newVal <- executeRelatedAction actionName rref v
  assign appianValue newVal

clearAllExceptions :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
clearAllExceptions v = forGridRows_ sendUpdates (^. gfColumns . at "Exception Name" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) clearExceptions v
                       >>= forGridRows_ sendUpdates (^. gfColumns . at "Exception Name" . traverse . _TextCellDynLink . _2) (MonadicFold $ dropping 1 getGridFieldCell . traverse) clearExceptions
                       >>= forGridRows_ sendUpdates (^. gfColumns . at "Exception Name" . traverse . _TextCellDynLink . _2) (MonadicFold $ dropping 2 getGridFieldCell . traverse) clearExceptions

clearExceptions :: (RapidFire m, MonadGen m) => DynamicLink -> GridField GridFieldCell -> Value -> AppianT m Value
clearExceptions dyl _ v = sendUpdates' "Click on Exceptions Link" (MonadicFold $ to (const dyl) . to toUpdate . to Right) v
  >>= handleValidations
        (sendUpdates "Click 'Add Comment'" (MonadicFold $ to $ dynamicLinkUpdate "Add Comment")
         >=> sendUpdates' "Enter Review Note & Clear Exceptions" (paragraphArbitraryUpdate "Review Notes" 4000
                                              <|> MonadicFold (to $ buttonUpdate "Clear Exception")
                                             )
         >=> handleValidations pure
        )
  >>= sendUpdates "Click Go Back" (MonadicFold (to $ buttonUpdate "Go Back"))
  where
    handleValidations _ (Left (se, _)) = case se ^? _ValidationsError . runFold ((,) <$> Fold _1 <*> Fold _2) of
      Just (["You can only take action on Pending Exceptions."], v) -> return v
      _ -> throwError se
    handleValidations f (Right v) = f v

-- clickApplication :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
-- clickApplication val = do
--   rref <- handleMissing "Record ref" val $ val ^? dropping 1 getGridFieldCell . traverse . gfColumns . at "Application Number" . traverse . _TextCellLink  . _2 . traverse
--   (_, v) <- viewRelatedActions val rref
--   executeRelatedAction "Manage Exceptions" rref v

addDecision :: (RapidFire m, MonadGen m) => PIAReviewerType -> Value -> AppianT m Value
addDecision PIAInitial v = sendUpdates "Click 'Add Decision'" (MonadicFold $ to $ buttonUpdate "Add Decision") v
  -- >>= sendUpdates "Select Decision" (MonadicFold (to $ dropdownUpdate "Select Decision" 2))
  >>= sendUpdates "Select Reason" (MonadicFold (to $ dropdownUpdate "Select Reason" 2))
  >>= sendUpdates "Add FCDL Comment" (MonadicFold $ to $ dynamicLinkUpdate "Add FCDL Comment")
  >>= sendUpdates "Select FCDL" (MonadicFold (to $ dropdownUpdate "FCDL Comments" 2)
                                <|> MonadicFold (hasType "ParagraphField" . _JSON . to (pgfValue .~ "FCDL Comment without special characters.") . to toUpdate . to Right)
                                )
  >>= sendUpdates "Save FCDL" (MonadicFold $ to $ buttonUpdate "Save FCDL")
addDecision _ v = pure v
  
selectReason :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectReason v = case has (getDropdown "Select Reason") v of
  True -> sendUpdates "Select Reason" (MonadicFold (to $ dropdownUpdate "Select Reason" 2)) v
  False -> return v
