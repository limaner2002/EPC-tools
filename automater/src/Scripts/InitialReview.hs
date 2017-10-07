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
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action.Reified
import qualified Data.Foldable as F
import Scripts.Common
import Scripts.ReviewCommon
import Scripts.Test


initialReview :: ReviewConf -> ReviewBaseConf -> Appian Value
initialReview conf baseConf = do
  (rid, v) <- myAssignedReport baseConf
  distributeLinks "Review Request Decision" rid conf v
    >>= addDecisions
    >>= sendUpdates "Continue to Add Notes" (MonadicFold $ to (buttonUpdate "Continue"))
    >>= addNotes
    >>= sendUpdates "Send to Next Reviewer" (MonadicFold $ to (buttonUpdate "Send to Next Reviewer"))

manageAppealDetails :: ReviewConf -> ReviewBaseConf -> Appian Value
manageAppealDetails conf baseConf = do
  (rid, v) <- myAssignedReport baseConf
  distributeLinks "Manage Appeal Details" rid conf v
    >>= sendUpdates "Select Type" (dropdownArbitraryUpdateF "What type of decision would you like to appeal?"
                                  <|> dropdownArbitraryUpdateF "Error Analysis"
                                  <|> dropdownUpdateF' "Appeal Type" RevAdminCorrection
                                  <|> dropdownArbitraryUpdateF "HS Type"
                                  <|> dropdownArbitraryUpdateF "Appeal Reason"
                                  <|> dropdownArbitraryUpdateF "Appeal Category"
                                  <|> MonadicFold (to $ buttonUpdate "Save & Close")
                                  )

adminInitial :: ReviewConf -> ReviewBaseConf -> Appian Value
adminInitial conf baseConf = manageAppealDetails conf baseConf >> initialReview conf baseConf

finalReview :: ReviewBaseConf -> ReviewConf -> Appian Value
finalReview bConf conf = do
  (rid, v) <- myAssignedReport bConf
  distributeLinks "Add Review Notes" rid conf v
    >>= addNotes
    >>= sendUpdates "Send to next reviewer" (buttonUpdateWith (\l -> l == "Send to Next Reviewer" || l == "Review Completed") "Could not find 'Send to Next Reviewer'/'Review Completed' button!")

buttonUpdateWith :: (Plated s, AsValue s, AsJSON s) => (Text -> Bool) -> t -> ReifiedMonadicFold m s (Either t Update)
buttonUpdateWith f msg = MonadicFold (failing (getButtonWith f . to toUpdate . to Right) (to (const $ Left msg)))

myAssignedReport :: ReviewBaseConf -> Appian (ReportId, Value)
myAssignedReport conf = do
  (rid, v) <- openReport "My Assigned Post-Commit Assignments"
  res <- editReport (PathPiece rid)
    >>= sendReportUpdates rid "Select Review Type" (dropdownUpdateF' "Review Type" (reviewType conf))
    >>= sendReportUpdates rid "Select Reviewer Type" (dropdownUpdateF' "Reviewer Type" (reviewerType conf))
    >>= sendReportUpdates rid "Select Reviewer Type" (dropdownUpdateF' "Funding Year" (fundingYear conf))
    >>= sendReportUpdates rid "Click Apply Filters" (MonadicFold (to (buttonUpdate "Apply Filters")))
    >>= sendReportUpdates rid "Sort by Age" (MonadicFold $ getGridFieldCell . traverse . to setAgeSort . to toUpdate . to Right)
  return (rid, res)

setAgeSort :: GridField a -> GridField a
setAgeSort = gfSelection . traverse . _NonSelectable . pgISort .~ Just [SortField "secondsSinceRequest" True]

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
    >>= (\v -> do
            v' <- sendUpdates "Select Rationale" (MonadicFold (to $ dropdownUpdate "Rationale_1_Dropdown" 2)) v
            case v ^? getButton "No" of
              Nothing -> return $ trace "No button doesn't exist." v'
              Just _ -> sendUpdates "Click 'No' for 'does this change the original FRN decision?'" (MonadicFold (to $ buttonUpdate "No")) v
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


