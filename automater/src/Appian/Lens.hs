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

getGridWidget :: (FromJSON a, Contravariant f, Applicative f) =>
                 (GridWidget a -> f (GridWidget a)) -> Value -> f Value
getGridWidget = hasType "GridWidget" . to fromJSON . traverse

getGridWidgetValue :: (Contravariant f, Applicative f) =>
                 (GridWidget Value -> f (GridWidget Value)) -> Value -> f Value
getGridWidgetValue = getGridWidget

getNonNullGridWidget :: (Contravariant f, Applicative f) =>
                 (GridWidget NonNullValue -> f (GridWidget NonNullValue)) -> Value -> f Value
getNonNullGridWidget = getGridWidget

getGridWidgetRecordRefs :: (Applicative f, Contravariant f) => Text -> (Text -> f Text) -> Value -> f Value
getGridWidgetRecordRefs column = getGridWidgetValue . gwVal . traverse . _2 . at column . traverse . key "links" . plate . key "_recordRef" . _String

getGridWidgetDynLink :: (Contravariant f, Applicative f) => Text -> (DynamicLink -> f DynamicLink) -> Value -> f Value
getGridWidgetDynLink column = getGridWidgetValue . gwVal . traverse . _2 . at column . traverse . key "links" . plate . _JSON

getTabButtonGroup :: (FromJSON a, Contravariant f, Applicative f) =>
                 (TabButtonGroup a -> f (TabButtonGroup a)) -> Value -> f Value
getTabButtonGroup = hasType "TabButtonGroup" . to fromJSON . traverse

getRecordDashboardTab :: (Contravariant f, Applicative f) =>
                 (TabButtonGroup Dashboard -> f (TabButtonGroup Dashboard)) -> Value -> f Value
getRecordDashboardTab = getTabButtonGroup . to toDashboards
  where
    toDashboards = tbgTabs %~ (\tabs -> tabs ^.. traverse . asValue . key "link" . key "dashboard" . _String . to Dashboard)

getRecordDashboard :: (Contravariant f, Applicative f, Plated s, AsValue s) =>
     Text -> (Dashboard -> f Dashboard) -> s -> f s
getRecordDashboard dashboardName = hasKeyValue "title" dashboardName . key "rel" . _String . to (stripPrefix "filter-") . traverse . to Dashboard

getDatePicker :: (Contravariant f, Applicative f, Plated s, AsValue s, AsJSON s) => Text -> (DatePicker -> f DatePicker) -> s -> f s
getDatePicker label = hasLabel label

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

gridWidgetRows :: (Contravariant f, Applicative f, AsValue t, FromJSON a) => ([a] -> f [a]) -> t -> f t
gridWidgetRows = key "contents" . plate . key "contents" . to fromJSON . traverse

toDict :: (Eq a, Hashable a) => [a] -> [b] -> HashMap a b
toDict headers row = mapFromList $ zip headers row

asValue :: (Functor f, Contravariant f, Profunctor p) => Optic' p f Value Value
asValue = to id

nnvToNullable :: (Profunctor p, Functor f, Contravariant f) => p Text (f Text) -> p NonNullValue (f NonNullValue)
nnvToNullable = nnv . nnfStr . to toNullable

getEmbedded :: (Applicative f, Plated s, AsValue s) => Text -> (Text -> f Text) -> s -> f s
getEmbedded label = hasKeyValue "name" label . key "children" . plate . _String

instance FromJSON a => FromJSON (GridWidget a) where
  parseJSON val = GridWidget <$> vals
    where
      vals = do
        res <- traverse checkColumns $ zip (repeat headers) rows
        case isSelectable of
          False -> return $ zip (repeat Nothing) $ fmap (uncurry toDict) res
          True -> return $ zip (fmap Just checkBoxes) $ fmap (uncurry toDict) res

      headers = val ^.. gridWidgetHeaders
      rows = val ^.. gridWidgetRows
      checkColumns (headers, row)
        | isSelectable && length headers == length row + 1 = return (drop 1 headers, row)
        | not isSelectable && length headers == length row = return (headers, row)
        | otherwise = fail "The number of headers does not match the number of rows!"
      isSelectable = case val ^? key "selectable" . _Bool of
        Nothing -> False
        Just b -> b
      checkBoxes :: [CheckboxGroup]
      checkBoxes = val ^.. deep (filtered $ has $ key "accessibilityLabel" . _String . prefixed "Select row") . _JSON . to (id :: CheckboxGroup -> CheckboxGroup)

instance FromJSON a => FromJSON (TabButtonGroup a) where
  parseJSON (Object o) = TabButtonGroup <$> o .: "tabs"
  parseJSON _ = fail "Could not parse TabButton Group. Expecting JSON Object but got something else."

instance FromJSON LinkRecordRef where
  parseJSON val = case val ^? key "link" . key "_recordRef" . _String of
    Nothing -> fail "Cannot find a recordRef link."
    Just ref -> return $ LinkRecordRef ref
