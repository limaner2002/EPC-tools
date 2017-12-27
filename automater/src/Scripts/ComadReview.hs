{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.Time
import Scripts.ComadReviewTypes
import Data.Random (MonadRandom)
import Control.Monad.Except

comadInitialReview :: (RapidFire m, MonadGen m) => ReviewBaseConf -> ComadReviewConf -> AppianT m Value
comadInitialReview baseConf conf = do
  (rid, v) <- myAssignedReport (conf ^? comadId) baseConf
  rref <- handleMissing "recordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  -- distributeLinks_ (\rid -> editAdjustmentAmounts rid >> reviewComadRequest rid) rid conf v
  editAdjustmentAmounts rref
  reviewComadRequest rref

editAdjustmentAmounts :: (RapidFire m, MonadGen m) => RecordRef -> AppianT m Value
editAdjustmentAmounts rid =
  viewRecordDashboard rid (Dashboard "summary")
    >>= flip viewRelatedActions rid
    >>= uncurry (executeRelatedAction "Edit Adjustment Amount")
    >>= forAdjustments
    >>= sendUpdates "Submit" (MonadicFold $ to $ buttonUpdate "Submit")

forAdjustments :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
forAdjustments = forGridRows_ sendUpdates (^. gfIdentifiers . traverse) (MonadicFold $ getGridFieldCell . traverse) (\gfi _ v -> editAdjustment v gfi)

pageAdjustments :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
pageAdjustments val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) editAdjustments val val

editAdjustments :: (RapidFire m, MonadGen m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
editAdjustments val gf = do
  v <- foldGridField' editAdjustment val gf
  return (v, v)

editAdjustment :: (RapidFire m, MonadGen m) => Value -> GridFieldIdent -> AppianT m Value
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

reviewComadRequest :: (RapidFire m, MonadGen m) => RecordRef -> AppianT m Value
reviewComadRequest rid =
  viewRecordDashboard rid (Dashboard "summary")
    >>= flip viewRelatedActions rid
    >>= uncurry (executeRelatedAction "Review COMAD Request")
    >>= forViolations
    >>= sendUpdates' "Continue to 'Review FRN Decisions'" (MonadicFold (to $ buttonUpdate "Continue"))
    >>= handleDecisionValidation
    >>= forDecisions
    >>= sendUpdates "Continue to 'Review Notes'" (MonadicFold $ to $ buttonUpdate "Continue")
    >>= addNotes
    >>= sendUpdates "Send to Next Reviewer" (MonadicFold $ to $ buttonUpdate "Send to Next Reviewer")

forViolations :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
forViolations = forGridRows_ sendUpdates (^. gfIdentifiers . traverse) (MonadicFold $ getGridFieldCell . traverse) (\gfi _ v -> addViolation v gfi)

pageViolations :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
pageViolations val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) addViolations val val

addViolations :: (RapidFire m, MonadGen m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
addViolations val gf = do
  v <- foldGridField' addViolation val gf
  return (v, v)

addViolation :: (RapidFire m, MonadGen m) => Value -> GridFieldIdent -> AppianT m Value
addViolation val ident = do
  gf <- handleMissing "FRN Violation Grid" val $ val ^? getGridFieldCell . traverse
  sendUpdates' "Violations: Select FRN Checkbox & Review Violations" (selectGridfieldUpdateF ident gf
                                                        <|> MonadicFold (to $ buttonUpdate "Review Violation(s) for FRN")
                                                        ) val
    >>= handleNoViolation
    
handleNoViolation :: (RapidFire m, MonadGen m) => Either ScriptError Value -> AppianT m Value
handleNoViolation (Left se) = case se ^? _ValidationsError . runFold ((,) <$> Fold _1 <*> Fold _2) of
  Just (["You cannot add a Violation to an FRN that you have marked as \"No Violation\""], v) -> return v
  Just ([], _) -> error "Violations not implemented yet!"
  _ -> throwError se
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

handleDecisionValidation :: MonadThrow m => Either ScriptError Value -> AppianT m Value
handleDecisionValidation (Left se) = case se ^? _ValidationsError . runFold ((,) <$> Fold (_1 . to (all $ isPrefixOf "You must select a Decision for FRN")) <*> Fold _2) of
  Just (True, v) -> return v
  _ -> throwError se
handleDecisionValidation (Right v) = return v

pageDecisions :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
pageDecisions val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) addDecisions val val

addDecisions :: (RapidFire m, MonadGen m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
addDecisions val gf = do
  v <- foldGridField' addDecision val gf
  return (v, v)

forDecisions :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
forDecisions = forGridRows_ sendUpdates (^. gfIdentifiers . traverse) (MonadicFold $ getGridFieldCell . traverse) (\gfi _ v -> addDecision v gfi)

addDecision :: (RapidFire m, MonadGen m) => Value -> GridFieldIdent -> AppianT m Value
addDecision val ident = do
  gf <- handleMissing "FRN Decision Grid" val $ val ^? getGridFieldCell . traverse
  eRes <- sendUpdates' "Decisions: Select FRN Checkbox" (selectGridfieldUpdateF ident gf) val
  val' <- case eRes of
    Right v -> return v
    Left se -> case se ^? _ValidationsError . _2 of
      Just v -> return v
      _ -> throwError se
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
addDecision_ :: (RapidFire m, MonadGen m) => DecisionState -> GridFieldIdent -> Value -> [Int] -> AppianT m Value
addDecision_ decState ident val [] = throwError $ MissingComponentError ("There are no available decisions to make! ident: " <> tshow ident, val)
addDecision_ decState ident val l = do
  gf <- handleMissing "FRN Decision Grid" val $ val ^? getGridFieldCell . traverse
  val' <- case decState of
    FirstTry -> return val
    Retry -> do
      eRes <- sendUpdates' "Decisions: Select FRN Checkbox" (selectGridfieldUpdateF ident gf) val
      case eRes of
        Right v -> return v
        Left se -> case se ^? _ValidationsError . _2 of
          Just v -> return v
          _ -> throwError se
      
  
  idx <- genArbitrary $ QC.elements l
  eRes <- sendUpdates' "Select & Save FRN Decision" (MonadicFold (to $ dropdownUpdate "FRN Decision" $ trace ("Making decision: " <> show idx) idx)
                                <|> MonadicFold (to $ buttonUpdate "Save Decision")
                              ) val'
  case eRes of
    Right val'' -> return val''
    -- Left ve -> addDecision_ Retry ident (ve ^. validationsExc . _2) (delete idx l)
    Left se -> case se ^? _ValidationsError . runFold ((,) <$> Fold (_1 . to (all $ isPrefixOf "You must select a Decision for FRN")) <*> Fold _2) of
                 Just (True, v) -> return v
                 Just (False, v) -> addDecision_ Retry ident v (delete idx l)

