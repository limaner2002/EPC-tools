{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

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

newtype AppianText = AppianText Text
  deriving (Show, Eq)

newtype AppianList a = AppianList [a]
  deriving Show

newtype AppianBoolean = AppianBoolean Bool
  deriving (Show, Eq)

newtype AppianUsername = AppianUsername Text
  deriving (Show, Eq)

data AppianPickerData
  = TypedText Text
  | Identifiers [AppianUsername]
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
  parseJSON _ = fail "Could not parse AppianInt"

instance FromJSON AppianString where
  parseJSON val@(Object o) = parseAppianType "string" mkAppianInt val
    where
      mkAppianInt = AppianString <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianString"

instance FromJSON AppianText where
  parseJSON val@(Object o) = parseAppianType "Text" mkAppianInt val
    where
      mkAppianInt = AppianText <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianText"

instance FromJSON AppianUsername where
  parseJSON val@(Object o) = parseAppianType "User" mkUser val
    where
      mkUser = AppianUsername <$> o .: "id"
  parseJSON _ = fail "Could not parse AppianUsername"

instance FromJSON AppianPickerData where
  parseJSON val@(Object o) = parseAppianType "PickerData" mkPicker val
    where
      mkPicker = (TypedText <$> o .: "typedText") <|> (Identifiers <$> o .: "identifiers")
  parseJSON _ = fail "Could not parse AppianPickerData"

instance ToJSON AppianInt where
  toJSON (AppianInt n) = object
    [ "#v" .= n
    , ("#t", "Integer")
    ]

instance ToJSON (AppianList Int) where
  toJSON (AppianList l) = object
    [ "#v" .= l
    , ("#t", "Integer?list")
    ]

instance ToJSON AppianString where
  toJSON (AppianString s) = object
    [ "#v" .= s
    , ("#t", "string")
    ]

instance ToJSON AppianText where
  toJSON (AppianText s) = object
    [ "#v" .= s
    , ("#t", "Text")
    ]

instance ToJSON AppianUsername where
  toJSON (AppianUsername un) = object
    [ "id" .= un
    , ("#t", "User")
    ]

instance ToJSON AppianPickerData where
  toJSON (TypedText txt) = object
    [ "typedText" .= txt
    , "identifiers" .= Array mempty
    , ("#t", "PickerData")
    ]

  toJSON (Identifiers idents) = object
    [ "identifiers" .= idents
    , ("#t", "PickerData")
    ]
  -- toJSON apd = object
  --   [ "identifiers" .= (apd ^. apdIdentifiers)
  --   , "typedText" .= (apd ^. apdTypedText)
  --   , ("#t", "PickerData")
  --   ]

instance FromJSON AppianBoolean where
  parseJSON val@(Object o) = parseAppianType "boolean" mkAppianInt val
    where
      mkAppianInt = AppianBoolean <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianBoolean"

data DropdownField = DropdownField
  { _dfClearIconLabel :: Text
  , _dfSaveInto :: [Text]
  , _dfValue :: Int
  , _dfWidth :: Text
  , _dfCid :: Text
  , _dfRequired :: Maybe Bool
  , _dfHasPlaceholderLabel :: Text
  , _dfChoices :: [Text]
  } deriving Show

data CheckboxGroup = CheckboxGroup
  { _cbgSaveInto :: [Text]
  , _cbgDisabled :: Bool
  , _cbgAccessibilityLabel :: Text
  , _cbgCid :: Text
  , _cbgKeyboard :: Maybe Text
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
  , _bwDisabled :: Maybe Bool
  } deriving Show

data UiConfig a = UiConfig
  { _uiContext :: AppianString
  , _uiUuid :: Text
  , _uiUpdates :: Maybe a
  } deriving Show

data TextField = TextField
  { _tfSaveInto :: [Text]
  , _tfValue :: Text
  , _tfCid :: Text
  , _tfRequired :: Maybe Bool
  , _tfKeyboard :: Text
  , _tfAlign :: Text
  , _tfPlaceholder :: Text
  , _tfRefreshAfter :: Text
  } deriving Show

data PickerWidget = PickerWidget
  { _pwMaxSelections :: Int
  , _pwSelectedTokensHint :: Text
  , _pwImageSize :: Text
  , _pwNumSuggestionsHint :: Text
  , _pwSaveInto :: [Text]
  , _pwNoResultsLabel :: Text
  , _pwSearchingLabel :: Text
  , _pwValue :: Value
  , _pwWidth :: Text
  , _pwCid :: Text
  , _pwRequired :: Bool
  , _pwFailedRequiredness :: Text
  , _pwSuggestions :: Value
  , _pwTestLabel :: Text
  , _pwPlaceholder :: Text
  } deriving Show

data ParagraphField = ParagraphField
  { _pgfHeight :: Text
  , _pgfSaveInto :: [Text]
  , _pgfValue :: Text
  , _pgfWidth :: Text
  , _pgfCid :: Text
  , _pgfPlaceholder :: Text
  , _pgfRefreshAfter :: Text
  } deriving Show

data DatePicker = DatePicker
  { _dpwNoneLabel :: Text
  , _dpwHelpTooltip :: Text
  , _dpwSaveInto :: [Text]
  , _dpwValue :: AppianDate
  , _dpwCid :: Text
  , _dpwRequired :: Bool
  , _dpwAlign :: Text
  , _dpwPlaceholder :: Text
  , _dpwTodayLabel :: Text
  } deriving Show

data DynamicLink = DynamicLink
  { _dylSaveInto :: [Text]
  , _dylValue :: AppianString
  , _dylAction :: Text
  , _dylCid :: Text
  , _dylTooltip :: Text
  , _dylConfirmButtonStyle :: Text
  , _dylValidate :: Text
  , _dylLabel :: Text
  } deriving Show

newtype AppianDate = AppianDate { _appianDate :: Maybe Day }
  deriving Show

newtype GridWidget a = GridWidget
  { _gwVal :: [(Maybe CheckboxGroup, HashMap Text a)]
  } deriving Show

newtype GridField a = GridField
  { _gfVal :: HashMap Text a
  } deriving Show

data GridFieldCell
  = TextCell (Vector Text)
  | TextCellLink (Vector Text, Vector RecordRef)
  deriving Show

newtype RecordRef = RecordRef Text
  deriving (Show, Eq)

newtype TabButtonGroup a = TabButtonGroup
  { _tbgTabs :: [a]
  } deriving Show

newtype NonNullString = NonNullString
  { _nnfStr :: NonNull Text
  } deriving Show

newtype NonNullValue = NonNullValue
  { _nnv :: NonNullString
  } deriving Show

newtype SaveRequestList update = SaveRequestList {_srlList :: [update]}
  deriving (Show, Eq)

newtype Update = Update { _getUpdateVal :: Value }
  deriving Show

newtype LinkRecordRef = LinkRecordRef
  { _recordRef :: Text
  } deriving (Show, Eq)

newtype Dashboard = Dashboard
  { _dsbVal :: Text
  } deriving (Show, Eq)

makeLenses ''DropdownField
makeLenses ''CheckboxGroup
makeLenses ''ButtonWidget
makeLenses ''UiConfig
makeLenses ''SaveRequestList
makeLenses ''Update
makeLenses ''TextField
makeLenses ''PickerWidget
makeLenses ''ParagraphField
makeLenses ''GridWidget
makeLenses ''GridField
makeLenses ''NonNullString
makeLenses ''NonNullValue
makeLenses ''TabButtonGroup
makeLenses ''LinkRecordRef
makeLenses ''DatePicker
makeLenses ''AppianDate
makeLenses ''DynamicLink

makePrisms ''GridFieldCell

instance FromJSON DropdownField where
  parseJSON val@(Object o) = parseAppianTypeWith (\typ -> isSuffixOf "DropdownField" typ || isSuffixOf "DropdownWidget" typ)  mkField val
    where
      mkField = DropdownField
        <$> o .: "clearIconLabel"
        <*> o .: "saveInto"
        <*> o .: "value"
        <*> o .: "width"
        <*> o .: "_cId"
        <*> o .:? "required"
        <*> o .: "hasPlaceholderLabel"
        <*> o .: "choices"
  parseJSON _ = fail "Could not parse DropdownField"

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
        <*> o .:? "keyboard"
        <*> o .: "align"
        <*> o .: "choices"
        <*> o .: "choiceLayout"
  parseJSON _ = fail "Could not parse CheckboxGroup"

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
        <*> o .:? "disabled"
  parseJSON _ = fail "Could not parse ButtonWidget"



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
    , "disabled" .= (bw ^. bwDisabled)
    ]

instance ToJSON a => ToJSON (SaveRequestList a) where
  toJSON srl = object
    [ ("#t", "SaveRequest?list")
    , "#v" .= (srl ^. srlList)
    ]

instance ToJSON TextField where
  toJSON tf = object
    [ "saveInto" .= (tf ^. tfSaveInto)
    , "value" .= (tf ^. tfValue)
    , "_cId" .= (tf ^. tfCid)
    , "required" .= (tf ^. tfRequired)
    , ("#t", "TextWidget")
    , "keyboard" .= (tf ^. tfKeyboard)
    , "align" .= (tf ^. tfAlign)
    , "placeholder" .= (tf ^. tfPlaceholder)
    , "refreshAfter" .= (tf ^. tfRefreshAfter)
    ]

instance ToJSON PickerWidget where
  toJSON pw = object
    [ "maxSelections" .= (pw ^. pwMaxSelections)
    , "selectedTokensHint" .= (pw ^. pwSelectedTokensHint)
    , "imageSize" .= (pw ^. pwImageSize)
    , "numSuggestionsHint" .= (pw ^. pwNumSuggestionsHint)
    , "saveInto" .= (pw ^. pwSaveInto)
    , "noResultsLabel" .= (pw ^. pwNoResultsLabel)
    , "searchingLabel" .= (pw ^. pwSearchingLabel)
    , "value" .= (pw ^. pwValue)
    , "width" .= (pw ^. pwWidth)
    , "_cId" .= (pw ^. pwCid)
    , "required" .= (pw ^. pwRequired)
    , "failedRequiredness" .= (pw ^. pwFailedRequiredness)
    , "suggestions" .= (pw ^. pwSuggestions)
    , "testLabel" .= (pw ^. pwTestLabel)
    , "placeholder" .= (pw ^. pwPlaceholder)
    , ("#t", "PickerWidget")
    ]

instance ToJSON ParagraphField where
  toJSON pgf = object
    [ "height" .= (pgf ^. pgfHeight)
    , "saveInto" .= (pgf ^. pgfSaveInto)
    , "value" .= (pgf ^. pgfValue)
    , "width" .= (pgf ^. pgfWidth)
    , "_cId" .= (pgf ^. pgfCid)
    , ("#t", "ParagraphWidget")
    , "placeholder" .= (pgf ^. pgfPlaceholder)
    , "refreshAfter" .= (pgf ^. pgfRefreshAfter)
    ]

instance ToJSON DatePicker where
  toJSON dpw = object
    [ "noneLabel" .= (dpw ^. dpwNoneLabel)
    , "helpTooltip" .= (dpw ^. dpwHelpTooltip)
    , "saveInto" .= (dpw ^. dpwSaveInto)
    , "value" .= (dpw ^. dpwValue)
    , "_cId" .= (dpw ^. dpwCid)
    , "required" .= (dpw ^. dpwRequired)
    , ("#t", "DatePickerWidget")
    , "align" .= (dpw ^. dpwAlign)
    , "placeholder" .= (dpw ^. dpwPlaceholder)
    , "todayLabel" .= (dpw ^. dpwTodayLabel)
    ]

instance ToJSON DynamicLink where
  toJSON dyl = object
    [ "saveInto" .= (dyl ^. dylSaveInto)
    , "value" .= (dyl ^. dylValue)
    , "action" .= (dyl ^. dylAction)
    , "_cId" .= (dyl ^. dylCid)
    , "tooltip" .= (dyl ^. dylTooltip)
    , "confirmButtonStyle" .= (dyl ^. dylConfirmButtonStyle)
    , "validate" .= (dyl ^. dylValidate)
    , "label" .= (dyl ^. dylLabel)
    , ("#t", "DynamicLink")
    ]

instance ToJSON AppianDate where
  toJSON apd = object
    [ ("#t", "date")
    , "#v" .= (apd ^. appianDate)
    ]

instance FromJSON a => FromJSON (SaveRequestList a) where
  parseJSON val@(Object o) = parseAppianType "SaveRequest?list" mkSaveRequestList val
    where
      mkSaveRequestList = SaveRequestList <$> o .: "#v"
  parseJSON _ = fail "Could not parse SaveRequestList"

instance ToJSON Update where
  toJSON up = toJSON $ up ^. getUpdateVal

instance FromJSON Update where
  parseJSON (Object o) = error "FromJSON for Update is not impletented yet!"
  parseJSON _ = fail "Could not parse Update"

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
  parseJSON _ = fail "Could not parse UiConfig"

instance FromJSON TextField where
  parseJSON val@(Object o) = parseAppianTypeWith (\t -> t == "TextField" || t == "TextWidget") mkField val
    where
      mkField = TextField
        <$> o .: "saveInto"
        <*> o .: "value"
        <*> o .: "_cId"
        <*> o .:? "required"
        <*> o .: "keyboard"
        <*> o .: "align"
        <*> o .: "placeholder"
        <*> o .: "refreshAfter"

instance FromJSON PickerWidget where
  parseJSON val@(Object o) = parseAppianTypeWith (\t -> t == "PickerWidget") mkWidget val
    where
      mkWidget = PickerWidget
        <$> o .: "maxSelections"
        <*> o .: "selectedTokensHint"
        <*> o .: "imageSize"
        <*> o .: "numSuggestionsHint"
        <*> o .: "saveInto"
        <*> o .: "noResultsLabel"
        <*> o .: "searchingLabel"
        <*> o .: "value"
        <*> o .: "width"
        <*> o .: "_cId"
        <*> o .: "required"
        <*> o .: "failedRequiredness"
        <*> o .: "suggestions"
        <*> o .: "testLabel"
        <*> o .: "placeholder"

instance FromJSON ParagraphField where
  parseJSON val@(Object o) = parseAppianTypeWith (== "ParagraphField") mkWidget val
    where
      mkWidget = ParagraphField
        <$> o .: "height"
        <*> o .: "saveInto"
        <*> o .: "value"
        <*> o .: "width"
        <*> o .: "_cId"
        <*> o .: "placeholder"
        <*> o .: "refreshAfter"

instance FromJSON Dashboard where
  parseJSON (Object o) = Dashboard <$> o .: "dashboard"

instance FromJSON NonNullString where
  parseJSON (String str) =
    case fromNullable str of
      Nothing -> fail "Cannot convert empty Text to NonNullString"
      Just nn -> return $ NonNullString nn
  parseJSON _ = fail "Decoding NonNullString: Expecting a JSON String"

instance FromJSON NonNullValue where
  parseJSON (Object o) = NonNullValue <$> o .: "value"
  parseJSON _ = fail "Decoding NonNullValue: Expecting a JSON Object"

instance FromJSON DatePicker where
  parseJSON val@(Object o) = parseAppianType "DatePickerField" mkDatePicker val
    where
      mkDatePicker = DatePicker
        <$> o .: "noneLabel"
        <*> o .: "helpTooltip"
        <*> o .: "saveInto"
        <*> o .: "value"
        <*> o .: "_cId"
        <*> o .: "required"
        <*> o .: "align"
        <*> o .: "placeholder"
        <*> o .: "todayLabel"

instance FromJSON DynamicLink where
  parseJSON val@(Object o) = parseAppianType "DynamicLink" mkDynLink val
    where
      mkDynLink = DynamicLink
        <$> o .: "saveInto"
        <*> o .: "value"
        <*> o .: "action"
        <*> o .: "_cId"
        <*> o .: "tooltip"
        <*> o .: "confirmButtonStyle"
        <*> o .: "validate"
        <*> o .: "label"

instance FromJSON AppianDate where
  parseJSON val@(Object o) = parseAppianType "date" mkDate val
    where
      mkDate = do
        mDateString <- o .:? "#v"
        case mDateString of
          Nothing -> return $ AppianDate Nothing
          Just dateString ->
            case stripSuffix "Z" dateString of
              Nothing -> fail $ "Decoding AppianDate: Invalid date format " <> show dateString
              Just d -> AppianDate <$> parseJSON (String d)

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

instance ToUpdate DropdownField where
  toUpdate df = Update $ object
    [ "_cId" .= (df ^. dfCid)
    , "model" .= df
    , "value" .= (AppianInt $ df ^. dfValue)
    , "saveInto" .= (df ^. dfSaveInto)
    , ("saveType", "PRIMARY")
    ]

instance ToUpdate CheckboxGroup where
  toUpdate cbg = Update $ object
    [ "_cId" .= (cbg ^. cbgCid)
    , "saveInto" .= (cbg ^. cbgSaveInto)
    , "value" .= (AppianList [1 :: Int])
    , ("saveType", "PRIMARY")
    , "model" .= cbg
    ]

instance ToUpdate TextField where
  toUpdate tf = Update $ object
    [ "_cId" .= (tf ^. tfCid)
    , "value" .= (AppianText $ tf ^. tfValue)
    , "saveInto" .= (tf ^. tfSaveInto)
    , ("saveType", "PRIMARY")
    ]

instance ToUpdate PickerWidget where
  toUpdate pw = Update $ object
    [ "saveInto" .= (pw ^. pwSaveInto)
    , "value" .= (pw ^. pwValue)
    , ("saveType", "PRIMARY")
    , "_cId" .= (pw ^. pwCid)
    , "model" .= pw
    ]

instance ToUpdate ParagraphField where
  toUpdate pgf = Update $ object
    [ "saveInto" .= (pgf ^. pgfSaveInto)
    , "value" .= AppianText (pgf ^. pgfValue)
    , ("saveType", "PRIMARY")
    , "_cId" .= (pgf ^. pgfCid)
    , "model" .= pgf
    ]

instance ToUpdate DatePicker where
  toUpdate dpw = Update $ object
    [ "saveInto" .= (dpw ^. dpwSaveInto)
    , "value" .= (dpw ^. dpwValue)
    , ("saveType", "PRIMARY")
    , "_cId" .= (dpw ^. dpwCid)
    , "model" .= dpw
    ]

instance ToUpdate DynamicLink where
  toUpdate dyl = Update $ object
    [ "saveInto" .= (dyl ^. dylSaveInto)
    , "value" .= (dyl ^. dylValue)
    , ("saveType", "PRIMARY")
    , "_cId" .= (dyl ^. dylCid)
    , "model" .= dyl
    , "label" .= (dyl ^. dylLabel)
    ]
