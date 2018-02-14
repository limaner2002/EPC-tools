{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Scripts.Assignment where

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
import Control.Monad.Time
import Data.Random (MonadRandom)
import Control.Monad.Except
import Appian.Internal.Updates
import Scripts.Execute
import Stats.CsvStream
import Appian.Internal.Arbitrary

assignment :: (RapidFire m, MonadGen m) => ReviewBaseConf -> ReviewConf' -> AppianT m Value
assignment baseConf conf = do
  runReportByName "Post-Commit Assignments" $ assignmentReport baseConf conf
  use appianValue
  
assignmentReport :: ReviewBaseConf -> ReviewConf' -> Report ()
assignmentReport baseConf conf = do
    let un = Identifiers [conf ^. reviewer . username . to AppianUsername]
    update "Select Review Type" (dropdownUpdateF' "Review Type" (reviewType baseConf))
    update "Select Reviewer Type" (dropdownUpdateF' "Reviewer Type" (reviewerType baseConf))
    update "Select Funding Year" (dropdownUpdateF' "Funding Year" (fundingYear baseConf))
    update "Enter the Application/Request Number" (MonadicFold (to $ textUpdate "Application/Request Number" $ conf ^. frnCaseNumber . caseNumber . to tshow))
    update "Click Apply Filters" (buttonUpdateF "Apply Filters")
    update "Select Application Number in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    update "Select & Assign Reviewer" (MonadicFold (to $ pickerUpdate "Select a Reviewer" un))
    update "Select Assign Case(s) to Reviewer" (MonadicFold (to (buttonUpdate "Assign Case(s) to Reviewer")))

runassignment :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> NumRecords -> IO () -- [Maybe (Either ServantError (Either ScriptError Value))]
runassignment conf = runScriptExhaustive $ assignment conf
