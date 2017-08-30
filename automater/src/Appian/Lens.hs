{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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

getTextField :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (TextField -> f TextField) -> s -> f s
getTextField val = hasKeyValue "label" val . _JSON

getPickerWidget :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (PickerWidget -> f PickerWidget) -> s -> f s
getPickerWidget val = hasKeyValue "label" val . _JSON

suffixed
  :: (Eq (Element a), IsSequence a, Choice p, Applicative f) =>
     a -> p () (f ()) -> p a (f a)
suffixed a = prism' (\() -> a) $ guard . (isSuffixOf a)

prefixed
  :: (Eq (Element a), IsSequence a, Choice p, Applicative f) =>
     a -> p () (f ()) -> p a (f a)
prefixed a = prism' (\() -> a) $ guard . (isPrefixOf a)

hasType :: (Applicative f, Plated s, AsValue s) =>
     Text -> Over (->) f s s s s
hasType = hasKeyValue "#t"

