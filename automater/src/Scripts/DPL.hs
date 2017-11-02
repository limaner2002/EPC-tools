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

launchAction :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Text -> AppianT m Value
launchAction actionName = do
  v <- actionsTab
  pid <- handleMissing ("Could not find process model id for action '" <> actionName <> "'") v $ v ^? hasKeyValue "displayLabel" actionName . key "processModelId" . _JSON . to ProcessModelId
  actionEx pid

data DPLType
  = DPLSelect
  | DPL1
  | DPL2
  | DPLFinal
  deriving (Show, Eq)

instance IsString DPLType where
  fromString "Please select a value" = DPLSelect
  fromString "First Demand Payment Letter" = DPL1
  fromString "Second Demand Payment Letter" = DPL2
  fromString "Final Demand Payment Letter" = DPLFinal

manageDPLs :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => DPLType -> FundingYear -> AppianT m Value
manageDPLs dplType fy = launchAction "Manage DPLs"
  >>= sendUpdates "Select DPL & Funding Year" (dropdownUpdateF' "Select DPL Type" dplType
                                               <|> dropdownUpdateF' "Funding Year" fy
                                               <|> MonadicFold (to $ buttonUpdate "Continue")
                                              )
  >>= sendUpdates "Continue to Preview" (MonadicFold (to $ buttonUpdate "Continue"))
  >>= sendUpdates "Send Notifications" (MonadicFold $ to $ buttonUpdate "Send Notifications")
