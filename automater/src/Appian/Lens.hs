{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Appian.Lens where

import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import Appian.Types
import Data.Aeson
import Data.Monoid (Endo)
import Appian.Instances (ActionId (..))

hasKey :: (AsValue s, Plated s, Applicative f) => Text -> Over (->) f s s s s
hasKey label = deep (filtered $ has $ key label) 

hasKeyValue :: (AsValue s, Plated s, Applicative f) => Text -> Text -> Over (->) f s s s s
hasKeyValue label val = deep (filtered $ has $ key label . _String . only val)

hasKeyValueWith :: (AsValue s, Plated s, Applicative f) => (Text -> Bool) -> Text -> Over (->) f s s s s
hasKeyValueWith f label = deep (filtered $ anyOf (key label . _String) f)

hasKeyValueRes :: (Applicative f, AsValue a, Plated a, Contravariant f) => Text -> Text -> Over (->) f (Result a) (Result a) (Result a) (Result a)
hasKeyValueRes label val = failing (_Success . hasKeyValue label val . to Success) id

hasLabel :: (AsJSON s, AsValue s, Plated s, Applicative f, FromJSON a, ToJSON a) => Text -> (a -> f a) -> s -> f s
hasLabel val = hasKeyValue "label" val . _JSON

hasLabelWith :: (AsJSON s, AsValue s, Plated s, Applicative f, FromJSON a, ToJSON a) => (Text -> Bool) -> (a -> f a) -> s -> f s
hasLabelWith f = hasKeyValueWith f "label" . _JSON

hasType :: (Applicative f, Plated s, AsValue s) =>
     Text -> Over (->) f s s s s
hasType = hasKeyValue "#t"

hasTypeWith :: (Contravariant f, Applicative f, FromJSON a) => (Text -> Bool) -> (Result a -> f (Result a)) -> Value -> f Value
hasTypeWith f = hasKeyValueWith f "#t" . to fromJSON

getButton :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (ButtonWidget -> f ButtonWidget) -> s -> f s
getButton label = deep (filtered (has $ runFold ((,) <$> Fold (key "confirmButtonStyle" . to (const ())) <*> Fold (key "label" . _String . only label)))) . _JSON

getButtonWith :: (AsJSON s, AsValue s, Plated s, Applicative f) => (Text -> Bool) -> (ButtonWidget -> f ButtonWidget) -> s -> f s
getButtonWith f = deep (filtered (has $ runFold ((,) <$> Fold (key "confirmButtonStyle" . to (const ())) <*> Fold (filtered $ anyOf (key "label" . _String) f)))) . _JSON

getDropdown :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (DropdownField -> f DropdownField) -> s -> f s
getDropdown = hasTypeAndLabel "DropdownField"

getDropdown' :: (Contravariant f, Applicative f) => Text -> (Result DropdownField -> f (Result DropdownField)) -> Value -> f Value
getDropdown' label = deep (getComponentWith (\v -> isDropdown v && hasLabel' label v))

getDropdownWith :: (Contravariant f, Applicative f) => (Value -> Bool) -> (Result DropdownField -> f (Result DropdownField)) -> Value -> f Value
getDropdownWith = getComponentWith

isDropdown :: Value -> Bool
isDropdown = anyOf (key "#t" . _String) (\t -> t == "DropdownField" || t == "DropdownWidget")

hasLabel' :: Text -> Value -> Bool
hasLabel' label v = has (key "label" . _String . only label) v || has (key "inlineLabel" . _String . only label) v

getComponentWith :: (Contravariant f, Applicative f, FromJSON a) => (Value -> Bool) -> (Result a -> f (Result a)) -> Value -> f Value
getComponentWith pred = filtered pred . to fromJSON

getTextField :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (TextField -> f TextField) -> s -> f s
getTextField = hasLabel

getPickerWidget :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (PickerWidget -> f PickerWidget) -> s -> f s
getPickerWidget label = failing byLabel byTestLabel . _JSON
  where
    byLabel = deep (filtered $ has $ runFold ((,) <$> Fold (filtered $ anyOf (key "#t" . _String) pickerType) <*> Fold (key "label" . _String . only label)))
    byTestLabel = deep (filtered $ has $ runFold ((,) <$> Fold (filtered $ anyOf (key "#t" . _String) pickerType) <*> Fold (key "testLabel" . _String . only ("test-" <> label)))) -- hasTypeAnd "PickerWidget" (key "testLabel" . _String . only ("test-" <> label))
    pickerType t = isSuffixOf "PickerWidget" t || isSuffixOf "PickerField" t

hasTypeAndLabel :: (AsJSON s, AsValue s, Plated s, Applicative f, FromJSON t, ToJSON t) => Text -> Text -> (t -> f t) -> s -> f s
hasTypeAndLabel typ label = deep (filtered $ has $ runFold ((,) <$> Fold (key "#t" . _String . suffixed typ) <*> Fold (key "label" . _String . only label))) . _JSON

getParagraphField :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (ParagraphField -> f ParagraphField) -> s -> f s
getParagraphField label = hasTypeAndLabel "ParagraphField" label

getCheckboxGroup :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (CheckboxGroup -> f CheckboxGroup) -> s -> f s
getCheckboxGroup label = hasTypeAndLabel "CheckboxField" label

getRadioButtonField :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (RadioButtonField -> f RadioButtonField) -> s -> f s
getRadioButtonField label = deep (filtered $ has $ runFold ((,) <$> Fold (key "#t" . _String . only "RadioButtonField") <*> Fold (key "label" . _String . only label))) . _JSON

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

getDynamicLink :: (AsJSON s, AsValue s, Plated s, Applicative f) => Text -> (DynamicLink -> f DynamicLink) -> s -> f s
getDynamicLink label = deep (filtered $ has $ runFold ((,) <$> Fold (key "#t" . _String . only "DynamicLink") <*> Fold (key "label" . _String . only label))) . _JSON

getGridField :: (FromJSON a, Contravariant f, Applicative f) => (Result (GridField a) -> f (Result (GridField a))) -> Value -> f Value
getGridField = hasTypeWith (isSuffixOf "GridField")

getGridFieldCell :: (Contravariant f, Applicative f) => (Result (GridField GridFieldCell) -> f (Result (GridField GridFieldCell))) -> Value -> f Value
getGridFieldCell = getGridField

getGridFieldValue :: (Contravariant f, Applicative f) => (Result (GridField Value) -> f (Result (GridField Value))) -> Value -> f Value
getGridFieldValue = getGridField

getGridFieldRecordRefs :: (Applicative f, Contravariant f) => Text -> (Vector RecordRef -> f (Vector RecordRef)) -> Value -> f Value
getGridFieldRecordRefs column = getGridFieldCell . traverse . gfColumns . at column . traverse . _TextCellLink . _2

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

gridWidgetHeaders :: (Applicative f, AsValue t) => (Text -> f Text) -> t -> f t
gridWidgetHeaders = key "columnHeaders" . plate . key "contents" . plate . key "label" . _String

gridWidgetRows :: (Contravariant f, Applicative f, AsValue t, FromJSON a) => ([a] -> f [a]) -> t -> f t
gridWidgetRows = key "contents" . plate . key "contents" . to fromJSON . traverse

gridFieldColumns :: (FromJSON a, Contravariant f, Applicative f, AsValue t) => ((Text, Result a) -> f (Text, Result a)) -> t -> f t
gridFieldColumns = key "columns" . plate . runFold ((,) <$> Fold (key "label" . _String) <*> Fold (to fromJSON))

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

instance FromJSON a => FromJSON (GridField a) where
  parseJSON val@(Object o) = case errors of
                    [] -> GridField dict
                      <$> o .:? "identifiers"
                      <*> o .:? "value"
                      <*> pure val
                      <*> o .: "saveInto"
                      <*> o .: "_cId"
                      <*> o .: "totalCount"
                    l -> fail $ intercalate "\n" l
    where
      results = val ^.. gridFieldColumns
      errors = results ^.. traverse . _2 . _Error
      dict = mapFromList $ results ^.. traverse . runFold ((,) <$> Fold _1 <*> Fold (_2 . _Success))

instance FromJSON GridFieldCell where
  parseJSON (Object o) =
        parseEmptyColumn
    <|> (TextCellLink <$> ((,) <$> o .: "data" <*> o .: "links"))
    <|> (TextCellDynLink <$> ((,) <$> o .: "data" <*> o .: "links"))
    <|> (TextCell <$> o .: "data")
    <|> (ImageColumn <$> o .: "data")
    where
      parseEmptyColumn = do
        (mData :: Maybe [Value]) <- o .:? "data"
        case mData of
          Nothing -> return EmptyColumn
          Just [] -> return EmptyColumn
          _ -> fail "Could not parse GridFieldCell"

  parseJSON _ = fail "Could not parse GridFieldCell: Expecting JSON Object but got something else."

instance FromJSON a => FromJSON (TabButtonGroup a) where
  parseJSON (Object o) = TabButtonGroup <$> o .: "tabs"
  parseJSON _ = fail "Could not parse TabButton Group. Expecting JSON Object but got something else."

instance FromJSON LinkRecordRef where
  parseJSON val = do
    let res = val ^. to pure . keyRes "link" . keyRes "_recordRef"
    case res of
      Error msg -> fail $ "Could not parse LinkRecordRef: " <> msg
      Success v -> LinkRecordRef <$> parseJSON v

keyRes :: (Contravariant f, Applicative f) => Text -> Over (->) f (Result Value) (Result Value) (Result Value) (Result Value)
keyRes i = failing (_Success . failing (_Object . ix i . to Success) (to (const $ Error $ "Missing key " <> show i))) (to id)

plateRes :: (Contravariant f, Plated a, Applicative f) => Over (->) f (Result a) (Result a) (Result a) (Result a)
plateRes = failing (_Success . plate . to Success) id

_Error :: (Choice p, Applicative f) =>
     p String (f String) -> p (Result t) (f (Result t))
_Error = prism' Error fromResult
  where
    fromResult (Error msg) = Just msg
    fromResult _ = Nothing

_Success :: (Choice p, Applicative f) => p a (f a) -> p (Result a) (f (Result a))
_Success = prism' Success fromResult
  where
    fromResult (Success x) = Just x
    fromResult _ = Nothing

instance FromJSON RecordRef where
  parseJSON (Object o) = RecordRef <$> o .: "_recordRef"

checkResult :: Result a -> Result [a] -> Result [a]
checkResult _ (Error msg) = Error msg
checkResult (Error msg) _ = Error msg
checkResult (Success x) (Success l) = Success (x : l)

parsed :: a -> Either Text [a] -> Either Text [a]
parsed _ (Left msg) = Left msg
parsed x (Right l) = Right (x : l)

                     -- It's possible to mix the old functions with this as long as there is a 'to pure' somewhere.
                     -- ex: x ^&.. _JSON . asValue . hasType "GridField" . to pure . keyRes "columns" . plateRes . keyRes "label"
(^&..) :: s -> Getting (Endo (Result [a])) s (Result a) -> Result [a]
s ^&.. l = f l s
  where
    f x = foldrOf x checkResult (Success mempty)

infixl 8 ^&..

testF :: (Applicative f, Choice p) => t -> (t -> t -> Bool) -> p () (f ()) -> p t (f t)
testF a f = prism' (\() -> a) $ guard . (f a)

getRelatedActionId :: (Contravariant f, Applicative f, Plated s, AsValue s) => Text -> (ActionId -> f ActionId) -> s -> f s
getRelatedActionId action = hasKeyValue "title" action . deep (hasKeyValue "title" "Execute related action") . key "href" . _String . to (lastMay . splitElem '/') . traverse . to ActionId
