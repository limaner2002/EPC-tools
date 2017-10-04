{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.CIPACertifications where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Reified
import Scripts.Common

       -- After "Edit FCC Form 486" Related Action
addCertifications :: Value -> Appian Value
addCertifications =
  sendUpdates "Continue to 'Early Filing and Waiver'" (MonadicFold $ to $ buttonUpdate "Continue")
    >>= sendUpdates "Continue to" (MonadicFold $ to $ buttonUpdate "Continue")
