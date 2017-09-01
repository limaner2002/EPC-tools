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

hasLabel :: (AsJSON s, AsValue s, Plated s, Applicative f, FromJSON a, ToJSON a) => Text -> (a -> f a) -> s -> f s
hasLabel val = hasKeyValue "label" val . _JSON

getButton :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (ButtonWidget -> f ButtonWidget) -> s -> f s
getButton = hasLabel

getDropdown :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (DropdownField -> f DropdownField) -> s -> f s
getDropdown = hasLabel

getTextField :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (TextField -> f TextField) -> s -> f s
getTextField = hasLabel

getPickerWidget :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (PickerWidget -> f PickerWidget) -> s -> f s
getPickerWidget = hasLabel

getParagraphField :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (ParagraphField -> f ParagraphField) -> s -> f s
getParagraphField = hasLabel

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

gridWidgetHeaders :: (Applicative f, AsValue t) => (Text -> f Text) -> t -> f t
gridWidgetHeaders = key "columnHeaders" . plate . key "contents" . plate . key "label" . _String

asValue :: (Functor f, Contravariant f, Profunctor p) => Optic' p f Value Value
asValue = to id
