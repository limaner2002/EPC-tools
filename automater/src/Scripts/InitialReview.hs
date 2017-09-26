{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.InitialReview where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Control.Lens
import Control.Lens.Action.Reified
import qualified Data.Foldable as F
import Scripts.Common

initialReview :: Appian Value
initialReview = do
  v <- reportsTab
  rid <- getReportId "My Assigned Post-Commit Assignments" v
  editReport (PathPiece rid)
    >>= sendReportUpdates rid "Select Review Type" (MonadicFold (to (dropdownUpdate "Review Type" 6)))
    >>= sendReportUpdates rid "Select Reviewer Type" (MonadicFold (to (dropdownUpdate "Reviewer Type" 2)))
    >>= sendReportUpdates rid "Select Funding Year" (MonadicFold (to (dropdownUpdate "Funding Year" 2)))
    >>= sendReportUpdates rid "Click Apply Filters" (MonadicFold (to (buttonUpdate "Apply Filters")))
    >>= sendReportUpdates rid "Sort by Age" (MonadicFold $ getGridFieldCell . traverse . to setAgeSort . to toUpdate . to Right)
    >>= viewRelatedActions
    >>= uncurry (executeRelatedAction "Review Request Decision")
    >>= addDecisions

setAgeSort :: GridField a -> GridField a
setAgeSort = gfSelection . traverse . _NonSelectable . pgISort .~ Just [SortField "secondsSinceRequest" True]

viewRelatedActions :: Value -> Appian (RecordRef, Value)
viewRelatedActions v = do
  recordRef <- handleMissing "RecordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  let ref = PathPiece recordRef
  v' <- viewRecordDashboard ref (PathPiece $ Dashboard "summary")
  dashboard <- handleMissing "Related Actions Dashboard" v' $ v' ^? getRecordDashboard "Related Actions"
  v'' <- viewRecordDashboard ref (PathPiece dashboard)
  return (recordRef, v'')

executeRelatedAction :: Text -> RecordRef -> Value -> Appian Value
executeRelatedAction action recordId val = do
  aid <- PathPiece <$> (handleMissing ("could not find actionId for " <> tshow action) val $ val ^? getRelatedActionId action)
  relatedActionEx (PathPiece recordId) aid

-- res' <- tryAny $ runAppian (sequence $ foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) (\l gf -> pure $ l <> gf ^.. gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse) mempty <$> res ^? _Right . _Right) env login

addDecisions :: Value -> Appian Value
addDecisions val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) makeDecisions val val

makeDecisions :: Value -> GridField GridFieldCell -> Appian Value
makeDecisions val gf = foldGridField' (makeDecision gf) val gf

foldGridField' :: (b -> AppianInt -> Appian b) -> b -> GridField a -> Appian b
foldGridField' f b gf = do
  let boxes = gf ^.. gfIdentifiers . traverse . traverse
  F.foldlM f b boxes

selectCheckbox :: AppianInt -> GridField a -> GridField a
selectCheckbox ident = gfSelection . traverse . _Selectable . gslSelected .~ [ident]

makeDecision :: GridField a -> Value -> AppianInt -> Appian Value
makeDecision gf val ident =
  sendUpdates "Decision: Select FRN Checkbox & Review Decisions" (MonadicFold (failing (to (const (trace (show $ gf' ^. gfSelection) gf')) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))
                                                                  <|> MonadicFold (to $ buttonUpdate "Review FRN Decision(s)")
                                                                 ) val
    >>= sendUpdates "Click Add Decision" (MonadicFold $ to $ buttonUpdate "Add Decision")
    >>= sendUpdates "Select Decision" (MonadicFold $ to $ dropdownUpdate "Select Decision" 2)
    >>= sendUpdates "Select Reason" (MonadicFold $ to $ dropdownUpdate "Select Reason" 2)
    >>= sendUpdates "Save Decision (Should fail!)" (MonadicFold $ to $ buttonUpdate "Save Decision")
    where
      gf' = selectCheckbox ident gf
