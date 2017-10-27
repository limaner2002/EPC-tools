{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.FCCForm471 where

import ClassyPrelude
import Control.Lens hiding (index)
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Scripts.Test
import qualified Test.QuickCheck as QC
import Control.Lens.Action
import Control.Lens.Action.Reified
import Scripts.Common
import Control.Monad.Logger

newtype Form471Num = Form471Num Int
  deriving (Show, Eq, Num)

form471Certification :: RunClient m => Form471Num -> AppianT Value
form471Certification formNum = do
  tasksList <- tasksTab Nothing
  taskId <- hasKeyValueWith  "content" . key "id" . _String . to (stripPrefix "t-") . traverse
