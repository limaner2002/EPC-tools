{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Appian.Internal.Arbitrary
import Control.Monad.Time
import Data.Random (MonadRandom)
import Control.Monad.Except

initialReview :: (RapidFire m, MonadGen m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
initialReview baseConf conf = do
  (rid, v) <- myAssignedReport (conf ^? frnCaseNumber) baseConf
  rref <- handleMissing "Select FRN Case: Empty FRN Case Grid" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  viewRelatedActions v rref
    >>= executeRelatedAction "Review Request Decision" rref . snd
    >>= addDecisions
    >>= sendUpdates "Continue to Add Notes" (MonadicFold $ to (buttonUpdate "Continue"))
    >>= addNotes
    >>= sendUpdates "Send to Next Reviewer" (MonadicFold $ to (buttonUpdate "Send to Next Reviewer"))

manageAppealDetails :: (RapidFire m, MonadGen m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
manageAppealDetails baseConf conf = do
  (rid, v) <- myAssignedReport (conf ^? frnCaseNumber) baseConf
  rref <- handleMissing "recordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  viewRelatedActions v rref
    >>= executeRelatedAction "Manage Appeal Details" rref . snd
    >>= sendUpdates "Select Type" (dropdownArbitraryUpdateF "What type of decision would you like to appeal?"
                                  <|> dropdownArbitraryUpdateF "Error Analysis"
                                  <|> dropdownArbitraryUpdateF "Appeal Type"
                                  <|> dropdownArbitraryUpdateF "HS Type"
                                  <|> dropdownArbitraryUpdateF "Appeal Reason"
                                  <|> dropdownArbitraryUpdateF "Appeal Category"
                                  <|> MonadicFold (to $ buttonUpdate "Save & Close")
                                  )

adminInitial :: (RapidFire m, MonadGen m) => ReviewConf' -> ReviewBaseConf -> AppianT m Value
adminInitial conf baseConf = manageAppealDetails baseConf conf >> initialReview baseConf conf

finalReview :: (RapidFire m, MonadGen m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
finalReview bConf conf = do
  (rid, v) <- myAssignedReport (conf ^? frnCaseNumber) bConf
  rref <- handleMissing "Select FRN Case: Empty FRN Case Grid" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse
  viewRelatedActions v rref
    >>= executeRelatedAction "Add Review Notes" rref . snd
    >>= addNotes
    >>= sendUpdates "Send to next reviewer" (buttonUpdateWithF (\l -> l == "Send to Next Reviewer" || l == "Review Completed") "Could not find 'Send to Next Reviewer'/'Review Completed' button!")

addDecisions :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
addDecisions val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) makeDecisions val val

makeDecisions :: (RapidFire m, MonadGen m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
makeDecisions val gf = do
  v <- foldGridField' makeDecision val gf
  return (v, v)

makeDecision :: (RapidFire m, MonadGen m) => Value -> GridFieldIdent -> AppianT m Value
makeDecision val ident = do
  gf <- handleMissing "FRN Decision Grid" val $ val ^? getGridFieldCell . traverse
  assign appianValue val
  sendUpdates1 "Decision: Select FRN Checkbox" (MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update")))
  sendUpdates1 "Review FRN Decisions" (buttonUpdateF "Review FRN Decision(s)")
  sendUpdates1 "Click Add Decision" (buttonUpdateF "Add Decision")
    -- The below is commented out to avoid an expression evaluation error regarding the decision template.
  -- sendUpdates1 "Select Decision" (dropdownArbitraryUpdateF "Select Decision")
  sendUpdates1 "Select Decision" (dropdownUpdateF1 "Select Decision" "Approved")
  sendUpdates1 "Select Reason" (dropdownArbitraryUpdateF "Select Reason")
  sendUpdates1 "Add Rationale" (dynamicLinkUpdateF "Add Rationale")
  sendUpdates1 "Select Rationale" (dropdownArbitraryUpdateF "Rationale_1_Dropdown")

  hasNoButton <- usesValue (has $ getButton "No")
  case hasNoButton of
    True -> sendUpdates1 "Click 'No' for 'does this change the original FRN decision?'" (buttonUpdateF "No")
    False -> return ()

  sendUpdates1 "Save Decision" (buttonUpdateF "Save Decision")
  use appianValue

  -- _ <- sendUpdates "Decision: Select FRN Checkbox" (MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))
  --                                                                ) val
  -- sendUpdates "Review FRN Decisions" (MonadicFold (to $ buttonUpdate "Review FRN Decision(s)")) val
  --   >>= sendUpdates "Click Add Decision" (MonadicFold $ to $ buttonUpdate "Add Decision")
  --   >>= sendUpdates "Select Decision" (MonadicFold $ to $ dropdownUpdate "Select Decision" 2)
  --   >>= sendUpdates "Select Reason" (MonadicFold $ to $ dropdownUpdate "Select Reason" 2)
  --   >>= sendUpdates "Add Rationale" (MonadicFold $ to $ dynamicLinkUpdate "Add Rationale")
  --   >>= (\v -> do
  --           v' <- sendUpdates "Select Rationale" (MonadicFold (to $ dropdownUpdate "Rationale_1_Dropdown" 2)) v
  --           case v ^? getButton "No" of
  --             Nothing -> return $ trace "No button doesn't exist." v'
  --             Just _ -> sendUpdates "Click 'No' for 'does this change the original FRN decision?'" (MonadicFold (to $ buttonUpdate "No")) v
  --       )
  --   >>= sendUpdates "Save Decision" (MonadicFold $ to $ buttonUpdate "Save Decision")
