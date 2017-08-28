{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Appian.Types where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types
import Control.Lens hiding ((.=))
import Control.Arrow ((>>>), arr)

newtype AppianInt = AppianInt Int
  deriving (Show, Eq, Num)

newtype AppianString = AppianString Text
  deriving (Show, Eq)

newtype AppianList a = AppianList [a]
  deriving Show

newtype AppianBoolean = AppianBoolean Bool
  deriving (Show, Eq)

parseAppianTypeWith :: Monad m => (Text -> Bool) -> m a -> Value -> m a
parseAppianTypeWith pred f (Object o) =
  case lookup "#t" o of
    Nothing -> fail "This Appian Type does not have an associated type with it."
    Just (String s) ->
      case pred s of
        False -> fail $ "Wrong Appian type: received " <> show s
        True -> f
    Just _ -> fail "Expected string"

parseAppianType :: Monad m => Text -> m a -> Value -> m a
parseAppianType t = parseAppianTypeWith (isSuffixOf t)

instance FromJSON AppianInt where
  parseJSON val@(Object o) = parseAppianType "int" mkAppianInt val
    where
      mkAppianInt = AppianInt <$> o .: "#v"

instance FromJSON AppianString where
  parseJSON val@(Object o) = parseAppianType "string" mkAppianInt val
    where
      mkAppianInt = AppianString <$> o .: "#v"

instance ToJSON AppianInt where
  toJSON (AppianInt n) = object
    [ "#v" .= n
    , ("#t", "Integer")
    ]

instance ToJSON AppianString where
  toJSON (AppianString s) = object
    [ "#v" .= s
    , ("#t", "string")
    ]

instance FromJSON AppianBoolean where
  parseJSON val@(Object o) = parseAppianType "boolean" mkAppianInt val
    where
      mkAppianInt = AppianBoolean <$> o .: "#v"

data DropdownField = DropdownField
  { _dfClearIconLabel :: Text
  , _dfSaveInto :: [Text]
  , _dfValue :: Int
  , _dfWidth :: Text
  , _dfCid :: Text
  , _dfRequired :: Bool
  , _dfHasPlaceholderLabel :: Text
  , _dfChoices :: [Text]
  } deriving Show

data CheckboxGroup = CheckboxGroup
  { _cbgSaveInto :: [Text]
  , _cbgDisabled :: Bool
  , _cbgAccessibilityLabel :: Text
  , _cbgCid :: Text
  , _cbgKeyboard :: Text
  , _cbgAlign :: Text
  , _cbgChoices :: [Text]
  , _cbgChoiceLayout :: Text
  } deriving Show

data ButtonWidget = ButtonWidget
  { _bwStyle :: Text
  , _bwSaveInto :: [Text]
  , _bwSize :: Text
  , _bwValue :: AppianString
  , _bwIcon :: Text
  , _bwAction :: Maybe Text
  , _bwCancelButtonLabel :: Text
  , _bwCid :: Text
  , _bwConfirmButtonLabel :: Text
  , _bwIsFlat :: Text
  , _bwAriaLabel :: Text
  , _bwConfirmHeader :: Text
  , _bwConfirmButtonStyle :: Text
  , _bwValidate :: Maybe Text
  , _bwLabel :: Text
  , _bwConfirmMessage :: Text
  , _bwButtonStyle :: Text
  } deriving Show

data UiConfig a = UiConfig
  { _uiContext :: AppianString
  , _uiUuid :: Text
  , _uiUpdates :: Maybe a
  } deriving Show

newtype SaveRequestList update = SaveRequestList {_srlList :: [update]}
  deriving (Show, Eq)

newtype Update = Update { _getUpdateVal :: Value }
  deriving Show

makeLenses ''DropdownField
makeLenses ''CheckboxGroup
makeLenses ''ButtonWidget
makeLenses ''UiConfig
makeLenses ''SaveRequestList
makeLenses ''Update

instance FromJSON DropdownField where
  parseJSON val@(Object o) = parseAppianTypeWith (\typ -> isSuffixOf "DropdownField" typ || isSuffixOf "DropdownWidget" typ)  mkField val
    where
      mkField = DropdownField
        <$> o .: "clearIconLabel"
        <*> o .: "saveInto"
        <*> o .: "value"
        <*> o .: "width"
        <*> o .: "_cId"
        <*> o .: "required"
        <*> o .: "hasPlaceholderLabel"
        <*> o .: "choices"

instance ToJSON DropdownField where
  toJSON df = object
    [ "clearIconLabel" .= (df ^. dfClearIconLabel)
    , "saveInto" .= (df ^. dfSaveInto)
    , "value" .= (df ^. dfValue)
    , "width" .= (df ^. dfWidth)
    , "_cId" .= (df ^. dfCid)
    , "required" .= (df ^. dfRequired)
    , "hasPlaceholderLabel" .= (df ^. dfHasPlaceholderLabel)
    , "choices" .= (df ^. dfChoices)
    , ("#t", "DropdownWidget")
    ]

instance FromJSON CheckboxGroup where
  parseJSON (Object o) = mkField
    where
      mkField = CheckboxGroup
        <$> o .: "saveInto"
        <*> o .: "disabled"
        <*> o .: "accessibilityLabel"
        <*> o .: "_cId"
        <*> o .: "keyboard"
        <*> o .: "align"
        <*> o .: "choices"
        <*> o .: "choiceLayout"

instance ToJSON CheckboxGroup where
  toJSON cbg = object
    [ "saveInto" .= (cbg ^. cbgSaveInto)
    , "disabled" .= (cbg ^. cbgDisabled)
    , "accessibilityLabel" .= (cbg ^. cbgAccessibilityLabel)
    , "_cId" .= (cbg ^. cbgCid)
    , "align" .= (cbg ^. cbgAlign)
    , "choices" .= (cbg ^. cbgChoices)
    , "choiceLayout" .= (cbg ^. cbgChoiceLayout)
    , ("#t", "CheckboxGroup")
    ]

instance FromJSON ButtonWidget where
  parseJSON (Object o) = mkField
    where
      mkField = ButtonWidget
        <$> o .: "style"
        <*> o .: "saveInto"
        <*> o .: "size"
        <*> o .: "value"
        <*> o .: "icon"
        <*> o .:? "action"
        <*> o .: "cancelButtonLabel"
        <*> o .: "_cId"
        <*> o .: "confirmButtonLabel"
        <*> o .: "isFlat"
        <*> o .: "ariaLabel"
        <*> o .: "confirmHeader"
        <*> o .: "confirmButtonStyle"
        <*> o .:? "validate"
        <*> o .: "label"
        <*> o .: "confirmMessage"
        <*> o .: "buttonStyle"

instance ToJSON ButtonWidget where
  toJSON bw = object
    [ "style" .= (bw ^. bwStyle)
    , "saveInto" .= (bw ^. bwSaveInto)
    , "size" .= (bw ^. bwSize)
    , "value" .= (bw ^. bwValue)
    , "icon" .= (bw ^. bwIcon)
    , "action" .= (bw ^. bwAction)
    , "cancelButtonLabel" .= (bw ^. bwCancelButtonLabel)
    , "_cId" .= (bw ^. bwCid)
    , ("#t", "ButtonWidget")
    , "confirmButtonLabel" .= (bw ^. bwConfirmButtonLabel)
    , "isFlat" .= (bw ^. bwIsFlat)
    , "ariaLabel" .= (bw ^. bwAriaLabel)
    , "confirmHeader" .= (bw ^. bwConfirmHeader)
    , "confirmButtonStyle" .= (bw ^. bwConfirmButtonStyle)
    , "validate" .= (bw ^. bwValidate)
    , "label" .= (bw ^. bwLabel)
    , "confirmMessage" .= (bw ^. bwConfirmMessage)
    , "buttonStyle" .= (bw ^. bwButtonStyle)
    ]

instance ToJSON a => ToJSON (SaveRequestList a) where
  toJSON srl = object
    [ ("#t", "SaveRequest?list")
    , "#v" .= (srl ^. srlList)
    ]

instance FromJSON a => FromJSON (SaveRequestList a) where
  parseJSON val@(Object o) = parseAppianType "SaveRequest?list" mkSaveRequestList val
    where
      mkSaveRequestList = SaveRequestList <$> o .: "#v"

instance ToJSON Update where
  toJSON up = toJSON $ up ^. getUpdateVal
  -- toJSON up = object
  --   [ "saveInto" .= (up ^. upSaveInto)
  --   , "value" .= (up ^. upVal)
  --   , "saveType" .= (up ^. upSaveType)
  --   , "_cId" .= (up ^. upCid)
  --   , "model" .= (up ^. upModel)
  --   , "label" .= (up ^. upLabel)
  --   , "blocking" .= (up ^. upBlocking)
  --   ]

instance FromJSON Update where
  parseJSON (Object o) = error "FromJSON for Update is not impletented yet!"
  -- parseJSON (Object o) = Update
  --   <$> o .: "_cId"
  --   <*> o .: "label"
  --   <*> o .: "model"
  --   <*> o .: "value"
  --   <*> o .: "saveInto"
  --   <*> o .: "saveType"
  --   <*> o .: "blocking"

instance ToJSON a => ToJSON (UiConfig a) where
  toJSON ui = object
    [ "context" .= (ui ^. uiContext)
    , "uuid" .= (ui ^. uiUuid)
    , ("#t", "UiConfig")
    , "updates" .= (ui ^. uiUpdates)
    ]

instance FromJSON a => FromJSON (UiConfig a) where
  parseJSON val@(Object o) = parseAppianType "UiConfig" mkConfig val
    where
      mkConfig = UiConfig
        <$> o .: "context"
        <*> o .: "uuid"
        <*> o .:? "updates"

      -- Util Functions
capitalize :: Textual a => a -> a
capitalize = (toUpper . take 1 &&& drop 1 >>> arr (uncurry mappend))

toLenses :: Text -> [Text] -> [Text]
toLenses prefix = fmap (prefix <>) . fmap capitalize

toAesonObjList :: Text -> [Text] -> [Text]
toAesonObjList varName labels = fmap (\(label, lens) -> tshow label <> " .= (" <> varName <> " ^. " <> lens <> ")") $ zip labels lenses
  where
    lenses = toLenses varName labels

toAesonObjString :: Text -> [Text] -> Text
toAesonObjString varName = intercalate "\n, " . toAesonObjList varName

toParser :: [Text] -> [Text]
toParser = fmap (\t -> "<*> o .: " <> tshow t)

class ToUpdate a where
  toUpdate :: a -> Update

instance ToUpdate ButtonWidget where
  toUpdate bw = Update $ object
    [ "_cId" .= (bw ^. bwCid)
    , "label" .= (bw ^. bwLabel)
    , "model" .= bw
    , "value" .= (bw ^. bwValue)
    , "saveInto" .= (bw ^. bwSaveInto)
    , ("saveType", "PRIMARY")
    , ("blocking", Bool False)
    ]

-- instance ToUpdate AppianString ButtonWidget where
--   toUpdate bw = Update
--     (bw ^. bwCid )
--     (bw ^. bwLabel)
--      bw
--     (bw ^. bwValue)
--     (bw ^. bwSaveInto)
--     "PRIMARY"
--     False

instance ToUpdate DropdownField where
  toUpdate df = Update $ object
    [ "_cId" .= (df ^. dfCid)
    , "model" .= df
    , "value" .= (AppianInt $ df ^. dfValue)
    , "saveInto" .= (df ^. dfSaveInto)
    , ("saveType", "PRIMARY")
    ]
