{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.Random where

import ClassyPrelude
import Data.Aeson.Lens
import Control.Lens
import Data.Aeson
import Appian.Lens
import Appian
import Appian.Client
import Scripts.Test
import Appian.Types
import Data.Proxy
import qualified Test.QuickCheck as QC
import Control.Lens.Action.Reified
import Control.Lens.Action

data InputField
  = Textbox TextField
  | Checkbox CheckboxField
  | Radio RadioButtonField
  | Picker PickerWidget
  | Date DatePicker
  | Button ButtonWidget
  | Dropdown DropdownField
  | Paragraph ParagraphField
  deriving Show

instance ToUpdate InputField where
  toUpdate (Textbox tf) = toUpdate tf
  toUpdate (Checkbox cb) = toUpdate cb
  toUpdate (Radio rd) = toUpdate rd
  toUpdate (Picker pk) = toUpdate pk
  toUpdate (Date dt) = toUpdate dt
  toUpdate (Button bt) = toUpdate bt
  toUpdate (Dropdown dd) = toUpdate dd
  toUpdate (Paragraph pg) = toUpdate pg

hasKeyValueWith' :: (Text -> Bool) -> Text -> Fold Value Value
hasKeyValueWith' f label = filtered $ anyOf (key label . _String) f

componentTypes :: HashMap Text (Value -> Result InputField)
componentTypes = mapFromList
  [ ("TextField", parseIt Textbox)
  , ("DropdownField", parseIt Dropdown)
  , ("DropdownWidget", parseIt Dropdown)
  , ("RadioButtonField", parseIt Radio)
  , ("CheckboxField", parseIt Checkbox)
  , ("PickerWidget", parseIt Picker)
  , ("PickerField", parseIt Picker)
  , ("ParagraphField", parseIt Paragraph)
  , ("DatePickerField", parseIt Date)
  , ("ButtonWidget", parseIt Button)
  ]
  where
    parseIt f = fmap f . fromJSON

isEditable :: Value -> Bool
isEditable v = has (key "value") v
  && not (isReadOnly v)
  && not (isDisabled v)

isReadOnly :: Value -> Bool
isReadOnly = has (key "readOnly" . _Bool . only True)

isDisabled :: Value -> Bool
isDisabled = has (key "disabled" . _Bool . only True)

parseComponentType :: Value -> Result InputField
parseComponentType v = case v ^? key "#t" . _String of
  Nothing -> Error $ "This object has no associated type with it."
  Just typ -> parseComponentType_ typ v

parseComponentType_ :: Text -> Value -> Result InputField
parseComponentType_ componentType v = case lookup componentType componentTypes of
  Nothing -> Error $ show componentType <> " is not a known component type."
  Just f -> f v

arbitraryInput :: MonadGen m => InputField -> m (Either Text InputField)
arbitraryInput (Textbox tf) = Right . Textbox <$> fillTextField 7 tf
arbitraryInput (Checkbox _) = pure $ notImplemented "Checkbox"
arbitraryInput (Radio rb) = Right . Radio <$> radioArbitrarySelect rb
arbitraryInput (Picker _) = pure $ notImplemented "Picker"
arbitraryInput (Date _) = pure $ notImplemented "Date Picker"
arbitraryInput (Dropdown dd) = Right . Dropdown <$> dropdownArbitrarySelect dd
arbitraryInput (Paragraph pg) = Right . Paragraph <$> fillParagraph 2000 pg
arbitraryInput (Button btn) = pure $ Right $ Button btn

notImplemented :: Text -> Either Text a
notImplemented fieldName = Left $ "Not implemented for " <> tshow fieldName <> " yet unfortunately."

arbitraryInputUpdateF :: MonadGen m => ReifiedMonadicFold m Value (Either Text Update)
arbitraryInputUpdateF = MonadicFold (cosmos . filtered isEditable . to parseComponentType . traverse . act arbitraryInput . to (fmap toUpdate))

sendArbitraryInput :: (MonadGen m, RapidFire m) => Int -> AppianT m (Either ScriptError ())
sendArbitraryInput maxRetries = go maxRetries =<< sendUpdates1' "Sending arbitrary input" arbitraryInputUpdateF
  where
    go _ (Right res) = return $ Right res
    go 0 res = return res
    go n (Left err) = go (n - 1) =<< sendUpdates1' "Resending arbitrary input" arbitraryInputUpdateF
