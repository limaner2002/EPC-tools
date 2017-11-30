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
import Control.Monad.Time

initialReview :: (RunClient m, MonadTime m, MonadGen m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
initialReview baseConf conf = do
  (rid, v) <- myAssignedReportTemp (conf ^? frnCaseNumber) baseConf
  -- distributeLinks "Review Request Decision" rid conf v
  rref <- handleMissing "recordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  viewRelatedActions v rref
    >>= executeRelatedAction "Review Request Decision" rref . snd
    >>= addDecisions
    >>= sendUpdates "Continue to Add Notes" (MonadicFold $ to (buttonUpdate "Continue"))
    >>= addNotes
    >>= sendUpdates "Send to Next Reviewer" (MonadicFold $ to (buttonUpdate "Send to Next Reviewer"))

manageAppealDetails :: (RunClient m, MonadTime m, MonadGen m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
manageAppealDetails baseConf conf = do
  (rid, v) <- myAssignedReportTemp (conf ^? frnCaseNumber) baseConf
  rref <- handleMissing "recordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  -- distributeLinks "Manage Appeal Details" rid conf v
  viewRelatedActions v rref
    >>= executeRelatedAction "Manage Appeal Details" rref . snd
    >>= sendUpdates "Select Type" (dropdownArbitraryUpdateF "What type of decision would you like to appeal?"
                                  <|> dropdownArbitraryUpdateF "Error Analysis"
--                                  <|> dropdownUpdateF' "Appeal Type" RevAdminCorrection
                                  <|> dropdownArbitraryUpdateF "Appeal Type"
                                  <|> dropdownArbitraryUpdateF "HS Type"
                                  <|> dropdownArbitraryUpdateF "Appeal Reason"
                                  <|> dropdownArbitraryUpdateF "Appeal Category"
                                  <|> MonadicFold (to $ buttonUpdate "Save & Close")
                                  )

adminInitial :: (RunClient m, MonadTime m, MonadGen m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m) => ReviewConf' -> ReviewBaseConf -> AppianT m Value
adminInitial conf baseConf = manageAppealDetails baseConf conf >> initialReview baseConf conf

finalReview :: (RunClient m, MonadTime m, MonadGen m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
finalReview bConf conf = do
  (rid, v) <- myAssignedReportTemp (conf ^? frnCaseNumber) bConf
  -- distributeLinks "Add Review Notes" rid conf v
  rref <- handleMissing "recordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  viewRelatedActions v rref
    >>= executeRelatedAction "Add Review Notes" rref . snd
    >>= addNotes
    >>= sendUpdates "Send to next reviewer" (buttonUpdateWith (\l -> l == "Send to Next Reviewer" || l == "Review Completed") "Could not find 'Send to Next Reviewer'/'Review Completed' button!")

addDecisions :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> AppianT m Value
addDecisions val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) makeDecisions val val

makeDecisions :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
makeDecisions val gf = do
  v <- foldGridField' makeDecision val gf
  return (v, v)

makeDecision :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> GridFieldIdent -> AppianT m Value
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
