{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.ComadReview where

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
import Scripts.ReviewCommon
import Scripts.Test
import qualified Test.QuickCheck as QC

comadInitialReview :: ReviewConf -> ReviewBaseConf -> Appian Value
comadInitialReview conf baseConf = do
  (rid, v) <- myAssignedReport baseConf
  distributeLinks_ (\rid -> editAdjustmentAmounts rid >> reviewComadRequest rid) rid conf v

editAdjustmentAmounts :: RecordRef -> Appian Value
editAdjustmentAmounts rid =
  viewRecordDashboard (PathPiece rid) (PathPiece $ Dashboard "summary")
    >>= flip viewRelatedActions rid
    >>= uncurry (executeRelatedAction "Edit Adjustment Amount")
    >>= pageAdjustments
    >>= sendUpdates "Submit" (MonadicFold $ to $ buttonUpdate "Submit")

pageAdjustments :: Value -> Appian Value
pageAdjustments val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) editAdjustments val val

editAdjustments :: Value -> GridField GridFieldCell -> Appian (Value, Value)
editAdjustments val gf = do
  v <- foldGridField' editAdjustment val gf
  return (v, v)

editAdjustment :: Value -> GridFieldIdent -> Appian Value
editAdjustment val ident = do
  gf <- handleMissing "FRN Adjustment Grid" val $ val ^? getGridFieldCell . traverse
  val' <- sendUpdates "Adjustments: Select FRN Checkbox" (selectGridfieldUpdateF ident gf) val
  let amtGen = QC.resize 50000 $ QC.arbitrarySizedNatural
      arbitraryAmt = intFieldArbitraryUpdateF_ (fillIntField_ amtGen)

  sendUpdates "Continue to Edit FRN Adjustment" (MonadicFold $ to $ buttonUpdate "Continue") val'
    >>= sendUpdates "Enter FRN Data" (arbitraryAmt "COMAD Overlap Amount"
                                     <|> arbitraryAmt "COMAD Recover Overlap"
                                     <|> arbitraryAmt "RIDF Overlap"
                                     <|> arbitraryAmt "COMAD/RIDF Overlap"
                                     <|> arbitraryAmt "FRN Cross-COMAD Request Overlap"
                                     <|> MonadicFold (checkboxGroupUpdate "Applicant RIDF Responsible Party" [1])
                                     <|> MonadicFold (checkboxGroupUpdate "Applicant COMAD Responsible Party" mempty)
                                     )
    >>= sendUpdates "Calculate & Save" ( arbitraryAmt "Applicant RIDF Amount"
                                         <|> MonadicFold (to $ buttonUpdate "Calculate")
                                         <|> MonadicFold (to $ buttonUpdate "Save")
                                       )

reviewComadRequest :: RecordRef -> Appian Value
reviewComadRequest rid =
  viewRecordDashboard (PathPiece rid) (PathPiece $ Dashboard "summary")
    >>= flip viewRelatedActions rid
    >>= uncurry (executeRelatedAction "Review COMAD Request")
    >>= pageViolations
    >>= sendUpdates' "Continue to 'Review FRN Decisions'" (MonadicFold (to $ buttonUpdate "Continue"))
    >>= handleDecisionValidation
    >>= pageDecisions
    >>= sendUpdates "Continue to 'Review Notes'" (MonadicFold $ to $ buttonUpdate "Continue")
    >>= addNotes
    >>= sendUpdates "Send to Next Reviewer" (MonadicFold $ to $ buttonUpdate "Send to Next Reviewer")

pageViolations :: Value -> Appian Value
pageViolations val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) addViolations val val

addViolations :: Value -> GridField GridFieldCell -> Appian (Value, Value)
addViolations val gf = do
  v <- foldGridField' addViolation val gf
  return (v, v)

addViolation :: Value -> GridFieldIdent -> Appian Value
addViolation val ident = do
  gf <- handleMissing "FRN Violation Grid" val $ val ^? getGridFieldCell . traverse
  sendUpdates' "Violations: Select FRN Checkbox & Review Violations" (selectGridfieldUpdateF ident gf
                                                        <|> MonadicFold (to $ buttonUpdate "Review Violation(s) for FRN")
                                                        ) val
    >>= handleNoViolation
    
handleNoViolation :: Either ValidationsException Value -> Appian Value
handleNoViolation (Left ve) = case ve ^. validationsExc . _1 of
  ["You cannot add a Violation to an FRN that you have marked as \"No Violation\""] -> return $ ve ^. validationsExc . _2
    -- sendUpdates "Continue to 'Review FRN Decisions'" (MonadicFold (to $ buttonUpdate "Continue")) $ ve ^. validationsExc . _2
  [] -> error "Violations not implemented yet!"
  _ -> throwM ve
handleNoViolation (Right v) =
  sendUpdates "Add Violation Button" (MonadicFold $ to $ buttonUpdate "Add Violation") v
    >>= sendUpdates "Add Violation" (MonadicFold $ dropping 1 getGridFieldCell . traverse . gfColumns . at "" . traverse . _TextCellDynLink . _2 . traverse . to toUpdate . to Right)
    >>= sendUpdates "Select Party at Fault & Save" (MonadicFold (to $ dropdownUpdate "Party at Fault" 2)
                                                    <|> MonadicFold (to $ buttonUpdate "Save & Continue")
                                                   )
    >>= sendUpdates "Add Rationale Button" (MonadicFold (to $ buttonUpdate "Add Rationale"))
    >>= sendUpdates "Select Rationale Type" (MonadicFold (to $ dropdownUpdate "Rationale Type" 3))
    >>= sendUpdates "Select Rationale Text" (MonadicFold $ dropping 2 getGridFieldCell . traverse . gfColumns . at "" . traverse . _TextCellDynLink . _2 . traverse . to toUpdate . to Right)
    >>= sendUpdates "Save Rationale" (MonadicFold $ to $ buttonUpdate "Save Rationale")
    >>= sendUpdates "Back to Violations" (MonadicFold $ to $ buttonUpdate "Back")
    >>= sendUpdates "Back to Review COMAD Request" (MonadicFold $ to $ buttonUpdate "Back")

handleDecisionValidation :: Either ValidationsException Value -> Appian Value
handleDecisionValidation (Left ve) = case ve ^. validationsExc . _1 of
  [msg] -> case isPrefixOf "You must select a Decision for FRN" msg of
    True -> return $ ve ^. validationsExc . _2
    False -> throwM ve
  _ -> throwM ve
handleDecisionValidation (Right v) = return v

pageDecisions :: Value -> Appian Value
pageDecisions val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) addDecisions val val

addDecisions :: Value -> GridField GridFieldCell -> Appian (Value, Value)
addDecisions val gf = do
  v <- foldGridField' addDecision val gf
  return (v, v)

addDecision :: Value -> GridFieldIdent -> Appian Value
addDecision val ident = do
  gf <- handleMissing "FRN Decision Grid" val $ val ^? getGridFieldCell . traverse
  eRes <- sendUpdates' "Decisions: Select FRN Checkbox" (selectGridfieldUpdateF ident gf) val
  val' <- case eRes of
    Right v -> return v
    Left ve -> return $ ve ^. validationsExc . _2
  df <- handleMissing "FRN Decision Dropdown" val' $ val' ^? getDropdown "FRN Decision"

  let choices
        | nChoices > 1 = [2..nChoices]
        | otherwise = mempty
      nChoices = df ^. dfChoices . to length

  addDecision_ FirstTry ident val' choices

data DecisionState
  = FirstTry
  | Retry

    -- Retries an arbitrary decision until one succeeds or there are none left to chose from.
addDecision_ :: DecisionState -> GridFieldIdent -> Value -> [Int] -> Appian Value
addDecision_ decState ident val [] = throwM $ MissingComponentException ("There are no available decisions to make! ident: " <> tshow ident, val)
addDecision_ decState ident val l = do
  gf <- handleMissing "FRN Decision Grid" val $ val ^? getGridFieldCell . traverse
  val' <- case decState of
    FirstTry -> return val
    Retry -> do
      eRes <- sendUpdates' "Decisions: Select FRN Checkbox" (selectGridfieldUpdateF ident gf) val
      case eRes of
        Right v -> return v
        Left ve -> return $ ve ^. validationsExc . _2
      
  
  idx <- liftIO $ generate $ QC.elements l
  eRes <- sendUpdates' "Select & Save FRN Decision" (MonadicFold (to $ dropdownUpdate "FRN Decision" $ trace ("Making decision: " <> show idx) idx)
                                <|> MonadicFold (to $ buttonUpdate "Save Decision")
                              ) val'
  case eRes of
    Right val'' -> return val''
    Left ve -> addDecision_ Retry ident (ve ^. validationsExc . _2) (delete idx l)

