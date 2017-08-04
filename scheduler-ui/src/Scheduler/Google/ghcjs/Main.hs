{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Reflex.Dom
import DriveUI
import SchedulerUI
import Types
import Common
import Prelude
import Scheduler.Types

main = mainWidgetWithHead Common.head $ do
  pb <- getPostBuild
  elAttr "div" ("class" =: "pure-u-1-1") $ schedulerWidget pb $ fmap asJMeterOpts
  elAttr "div" ("class" =: "pure-u-1-1") $ fileWidget pb

asJMeterOpts :: Job JMeterOpts -> Job JMeterOpts
asJMeterOpts = id
