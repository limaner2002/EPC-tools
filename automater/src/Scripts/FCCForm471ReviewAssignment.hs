{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FCCForm471ReviewAssignment where

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
import Control.Monad.Time
import Scripts.ReviewCommon
import Scripts.FCCForm471Common

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

instance IsString PIAReviewerType where
  fromString "PIA Initial Reviewer" = PIAInitial
  fromString "Heightened Scrutiny Initial Reviewer" = PIAHSInitial
  fromString "PIA Final Reviewer" = PIAFinal
  fromString "QA Reviewer" = PIAQA
  fromString "Heightened Scrutiny Final Reviewer" = PIAHSFinal
  fromString "QA Heightened Scrutiny" = PIAQAHS
  fromString "USAC QA Reviewer" = PIAUsacQA
  fromString "USAC Heightened Scrutiny Reviewer" = PIAUsacHS

newtype BEN = BEN Int
  deriving (Show, Eq, Num)

benToText :: BEN -> Text
benToText (BEN n) = tshow n

form471Assign :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => FundingYear -> PIAReviewerType -> BEN -> AppianUsername -> AppianT m Value
form471Assign fy revType ben username = do
  let un = Identifiers [username]
  (rid, v) <- openReport "471 Reviews Assignment "
  editReport rid
    >>= sendReportUpdates rid "Select Funding Year" (dropdownUpdateF' "Fund Year" fy)
    >>= sendReportUpdates rid "Enter BEN && Apply Filters" (MonadicFold (to $ textUpdate "BEN" $ benToText ben)
                                                           <|> MonadicFold (to $ buttonUpdate "Apply Filters")
                                                           )
    >>= sendReportUpdates rid "Sort by Application Number" (MonadicFold $ getGridFieldCell . traverse . to setAppNumSort . to toUpdate . to Right)
    >>= sendReportUpdates rid "Select Application" (MonadicFold $ getGridFieldCell . traverse . to (gridSelection [1]) . to toUpdate . to Right)
    >>= sendReportUpdates rid "Assign Reviewer" (MonadicFold (to $ pickerUpdate "Select a Reviewer" un)
                                                 <|> MonadicFold (to (buttonUpdate "Assign Application(s) to Reviewer"))
                                                )

setAppNumSort :: GridField a -> GridField a
setAppNumSort = gfSelection . traverse . failing _NonSelectable (_Selectable . gslPagingInfo) . pgISort .~ Just [SortField "applicationNumber" False]

gridSelection :: [Int] -> GridField a -> GridField a
gridSelection idxs gf = gfSelection . traverse . _Selectable . gslSelected .~ idents $ gf
  where
    idents = gf ^.. gfIdentifiers . traverse . itraversed . withIndex . filtered isDesired . _2
    isDesired (i, _) = i `elem` idxs

form471NumToText :: Form471Num -> Text
form471NumToText (Form471Num n) = tshow n

form471Review :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadGen m) => FundingYear -> PIAReviewerType -> Form471Num -> AppianT m Value
form471Review fy revType form471Num = do
  (rid, v) <- openReport "My Assigned 471 Applications"
  editReport rid
    >>= sendReportUpdates rid "Select Funding Year" (dropdownUpdateF' "Fund Year" fy)
    >>= sendReportUpdates rid "Enter Application Number & Apply Filters" (MonadicFold (to $ textUpdate "Application Number" (form471NumToText form471Num))
                                                                         <|> MonadicFold (to $ buttonUpdate "Apply Filters")
                                                                         )
    >>= clickApplication
    >>= clearAllExceptions
    >>= addDecision revType
    >>= sendUpdates "Complete Review Step" (MonadicFold $ getButtonWith (\l -> l == "Complete PIA Review" || l == "Complete HS Review") . to toUpdate . to Right)

clearAllExceptions :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadGen m) => Value -> AppianT m Value
clearAllExceptions v = forGridRows_ sendUpdates (^. gfColumns . at "Exception Name" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) clearExceptions v
                       >>= forGridRows_ sendUpdates (^. gfColumns . at "Exception Name" . traverse . _TextCellDynLink . _2) (MonadicFold $ dropping 1 getGridFieldCell . traverse) clearExceptions
                       >>= forGridRows_ sendUpdates (^. gfColumns . at "Exception Name" . traverse . _TextCellDynLink . _2) (MonadicFold $ dropping 2 getGridFieldCell . traverse) clearExceptions

clearExceptions :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadGen m) => DynamicLink -> GridField GridFieldCell -> Value -> AppianT m Value
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
    handleValidations _ (Left valExc) = case valExc ^. validationsExc . _1 of
      ["You can only take action on Pending Exceptions."] -> return $ valExc ^. validationsExc . _2
      _ -> throwM valExc
    handleValidations f (Right v) = f v

clickApplication :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> AppianT m Value
clickApplication val = do
  rref <- handleMissing "Record ref" val $ val ^? dropping 1 getGridFieldCell . traverse . gfColumns . at "Application Number" . traverse . _TextCellLink  . _2 . traverse
  (_, v) <- viewRelatedActions val rref
  executeRelatedAction "Manage Exceptions" rref v

addDecision :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadGen m) => PIAReviewerType -> Value -> AppianT m Value
addDecision PIAInitial v = sendUpdates "Click 'Add Decision'" (MonadicFold $ to $ buttonUpdate "Add Decision") v
  >>= sendUpdates "Select Decision" (MonadicFold (to $ dropdownUpdate "Select Decision" 2))
  >>= sendUpdates "Select Reason" (MonadicFold (to $ dropdownUpdate "Select Reason" 2))
  >>= sendUpdates "Add FCDL Comment" (MonadicFold $ to $ dynamicLinkUpdate "Add FCDL Comment")
  >>= sendUpdates "Select FCDL" (MonadicFold $ to $ dropdownUpdate "FCDL Comments" 2)
  >>= sendUpdates "Save FCDL" (MonadicFold $ to $ buttonUpdate "Save FCDL")
addDecision _ v = pure v
  
