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
import Scripts.Test

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
--     >>= addDecisions
    >>= sendUpdates "Continue to Add Notes" (MonadicFold $ to (buttonUpdate "Continue"))
    >>= addNotes
    >>= sendUpdates "Send to Next Reviewer" (MonadicFold $ to (buttonUpdate "Send to Next Reviewer"))

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

makeDecisions :: Value -> GridField GridFieldCell -> Appian (Value, Value)
makeDecisions val gf = do
  v <- foldGridField' makeDecision val gf
  return (v, v)

addNotes :: Value -> Appian Value
addNotes val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) makeNotes val val

makeNotes :: Value -> GridField GridFieldCell -> Appian (Value, Value)
makeNotes val gf = do
  v <- foldGridField' makeNote val gf
  return (v, v)

foldGridField' :: (b -> AppianInt -> Appian b) -> b -> GridField a -> Appian b
foldGridField' f b gf = do
  let boxes = gf ^.. gfIdentifiers . traverse . traverse
  F.foldlM f b boxes

selectCheckbox :: AppianInt -> GridField a -> GridField a
selectCheckbox ident = gfSelection . traverse . _Selectable . gslSelected .~ [ident]

makeDecision :: Value -> AppianInt -> Appian Value
makeDecision val ident = do
  gf <- handleMissing "FRN Decision Grid" val $ val ^? getGridFieldCell . traverse
  _ <- sendUpdates "Decision: Select FRN Checkbox" (MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))
                                                                 ) val
  sendUpdates "Review FRN Decisions" (MonadicFold (to $ buttonUpdate "Review FRN Decision(s)")) val
    >>= sendUpdates "Click Add Decision" (MonadicFold $ to $ buttonUpdate "Add Decision")
    >>= sendUpdates "Select Decision" (MonadicFold $ to $ dropdownUpdate "Select Decision" 2)
    >>= sendUpdates "Select Reason" (MonadicFold $ to $ dropdownUpdate "Select Reason" 2)
    >>= sendUpdates "Add Rationale" (MonadicFold $ to $ dynamicLinkUpdate "Add Rationale")
    >>= sendUpdates "Select Rationale" (MonadicFold (to $ dropdownUpdate "Rationale_1_Dropdown" 2)
                                        <|> MonadicFold (to $ buttonUpdate "No")
                                       )
    >>= sendUpdates "Save Decision" (MonadicFold $ to $ buttonUpdate "Save Decision")

makeNote :: Value -> AppianInt -> Appian Value
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
