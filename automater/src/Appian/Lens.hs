{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Appian.Lens where

import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import Appian.Types
import Data.Aeson

hasKey :: (AsValue s, Plated s, Applicative f) => Text -> Over (->) f s s s s
hasKey label = deep (filtered $ has $ key label) 

hasKeyValue :: (AsValue s, Plated s, Applicative f) => Text -> Text -> Over (->) f s s s s
hasKeyValue label val = deep (filtered $ has $ key label . _String . only val)

getButton :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (ButtonWidget -> f ButtonWidget) -> s -> f s
getButton val = hasKeyValue "label" val . _JSON

getDropdown :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (DropdownField -> f DropdownField) -> s -> f s
getDropdown val = hasKeyValue "label" val . _JSON
