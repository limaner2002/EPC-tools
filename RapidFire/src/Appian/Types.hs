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
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens hiding ((.=))
import Control.Arrow ((>>>), arr)

newtype AppianInteger = AppianInteger Int
  deriving (Show, Eq, Num)

newtype AppianInt = AppianInt Int
  deriving (Show, Eq, Num)

newtype AppianString = AppianString Text
  deriving (Show, Eq)

newtype AppianText = AppianText Text
  deriving (Show, Eq)

newtype AppianList a = AppianList [a]
  deriving (Show, Monoid)

newtype AppianBoolean = AppianBoolean Bool
  deriving (Show, Eq)

newtype AppianUsername = AppianUsername Text
  deriving (Show, Eq)

data AppianPickerData a
  = TypedText Text
  | Identifiers a
  deriving (Show, Eq)

parseAppianTypeWith :: Monad m => Text -> (Text -> Bool) -> m a -> Value -> m a
parseAppianTypeWith typ pred f (Object o) =
  case lookup "#t" o of
    Nothing -> fail "This Appian Type does not have an associated type with it."
    Just (String s) ->
      case pred s of
        False -> fail $ "Wrong Appian type: received " <> show s <> " expecting " <> show typ
        True -> f
    Just _ -> fail "Expected string"

parseAppianType :: Monad m => Text -> m a -> Value -> m a
parseAppianType t = parseAppianTypeWith t (isSuffixOf t)

instance FromJSON AppianInteger where
  parseJSON val@(Object o) = parseAppianType "Integer" mkAppianInteger val
    where
      mkAppianInteger = AppianInteger <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianInteger"

instance FromJSON AppianInt where
  parseJSON val@(Object o) = parseAppianType "int" mkAppianInteger val
    where
      mkAppianInteger = AppianInt <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianInt"

instance FromJSON AppianString where
  parseJSON val@(Object o) = parseAppianType "string" mkAppianInteger val
    where
      mkAppianInteger = AppianString <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianString"

instance FromJSON AppianText where
  parseJSON val@(Object o) = parseAppianType "Text" mkAppianInteger val
    where
      mkAppianInteger = AppianText <$> o .: "#v"
  parseJSON _ = fail "Could not parse AppianText"

instance FromJSON AppianUsername where
  parseJSON val@(Object o) = parseAppianType "User" mkUser val
    where
      mkUser = AppianUsername <$> o .: "id"
  parseJSON _ = fail "Could not parse AppianUsername"

instance FromJSON a => FromJSON (AppianPickerData a) where
  parseJSON val@(Object o) = parseAppianType "PickerData" mkPicker val
    where
      mkPicker = (TypedText <$> o .: "typedText") <|> (Identifiers <$> o .: "identifiers")
  parseJSON _ = fail "Could not parse AppianPickerData"

instance ToJSON AppianInteger where
  toJSON (AppianInteger n) = object
    [ "#v" .= n
    , ("#t", "Integer")
    ]

instance ToJSON AppianInt where
  toJSON (AppianInt n) = object
    [ "#v" .= n
    , ("#t", "int")
    ]

instance ToJSON (AppianList Int) where
  toJSON (AppianList l) = object
    [ "#v" .= l
    , ("#t", "Integer?list")
    ]

instance ToJSON (AppianList Text) where
  toJSON (AppianList l) = object
    [ "#v" .= l
    , ("#t", "Text?list")
    ]

instance ToJSON (AppianList Dictionary) where
  toJSON (AppianList l) = object
    [ "#v" .= l
    , ("#t", "Dictionary?list")
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

instance ToJSON a => ToJSON (AppianPickerData a) where
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
  parseJSON val@(Object o) = parseAppianType "boolean" mkAppianInteger val
    where
      mkAppianInteger = AppianBoolean <$> o .: "#v"
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
  , _dfInlineLabel :: Maybe Text
  } deriving Show

data CheckboxGroup = CheckboxGroup
  { _cbgSaveInto :: [Text]
  , _cbgDisabled :: Maybe Bool
  , _cbgAccessibilityLabel :: Maybe Text
  , _cbgCid :: Text
  , _cbgKeyboard :: Maybe Text
  , _cbgAlign :: Text
  , _cbgChoices :: [Text]
  , _cbgChoiceLayout :: Text
  , _cbgValue :: Maybe [Int]
  } deriving Show

data CheckboxField = CheckboxField
  { _cbfSaveInto :: [Text]
  , _cbfCid :: Text
  , _cbfAlign :: Text
  , _cbfChoices :: [Text]
  , _cbfChoiceLayout :: Text
  , _cbfValue :: Maybe [Int]
  } deriving Show

data RadioButtonField = RadioButtonField
  { _rdgSaveInto :: [Text]
  , _rdgCid :: Text
  , _rdgRequired :: Maybe Bool
  , _rdgChoices :: [Text]
  , _rdgChoiceLayout :: Text
  , _rdgValue :: Maybe AppianInteger
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
  , _uiIdentifier :: Maybe Identifier
  } deriving Show

data Identifier = Identifier
  { _idfSiteUrlStub :: Text
  , _idfUrlStub :: Text
  , _idfViewData :: Text
  , _idfView :: Text
  , _idfPageUrlStub :: Text
  }
  | DesignObject
    { _objOpaqueId :: Text
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
  , _tfReadOnly :: Maybe Bool
  } deriving Show

data PickerWidget = PickerWidget
  { _pwMaxSelections :: Maybe Int
  , _pwSelectedTokensHint :: Maybe Text
  , _pwImageSize :: Maybe Text
  , _pwNumSuggestionsHint :: Maybe Text
  , _pwSaveInto :: [Text]
  , _pwNoResultsLabel :: Maybe Text
  , _pwSearchingLabel :: Maybe Text
  , _pwValue :: Maybe Value
  , _pwWidth :: Maybe Text
  , _pwCid :: Text
  , _pwRequired :: Maybe Bool
  , _pwFailedRequiredness :: Text
  , _pwSuggestions :: Maybe Value
  , _pwTestLabel :: Maybe Text
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
  , _dpwRequired :: Maybe Bool
  , _dpwAlign :: Text
  , _dpwPlaceholder :: Text
  , _dpwTodayLabel :: Text
  } deriving Show

data DynamicLink = DynamicLink
  { _dylSaveInto :: [Text]
  , _dylValue :: AppianString
  , _dylAction :: Maybe Text
  , _dylCid :: Text
  , _dylTooltip :: Text
  , _dylConfirmButtonStyle :: Maybe Text
  , _dylValidate :: Maybe Text
  , _dylLabel :: Text
  } deriving Show

data GridValue
  = Selectable GridSelection
  | NonSelectable PagingInfo
  deriving Show

data GridSelection = GridSelection
  { _gslSelected :: [GridFieldIdent]
  , _gslPagingInfo :: PagingInfo
  } deriving Show

data PagingInfo = PagingInfo
  { _pgIBatchSize :: Int
  , _pgISort :: Maybe [SortField]
  , _pgIStartIndex :: Int
  } deriving Show

data SortField = SortField
  { _sfdField :: Text
  , _sfdAscending :: Maybe Bool
  } deriving Show

newtype AppianDate = AppianDate { _appianDate :: Maybe Day }
  deriving Show

-- Representation of a!gridLayout SAIL component
data GridWidget a = GridWidget
  { _gwVal :: [(Maybe CheckboxGroup, HashMap Text a)]
  , _gwActions :: Maybe (Vector DynamicLink)
  , _gwTotalCount :: Int
  } deriving Show

data GridField a = GridField
  { _gfColumns :: HashMap Text a
  , _gfIdentifiers :: Maybe (Vector GridFieldIdent)
  , _gfSelection :: Maybe GridValue
  , _gfModel :: Value
  , _gfSaveInto :: Maybe [Text]
  , _gfCid :: Text
  , _gfTotalCount :: Int
  }
  deriving Show

data ExpressionEditorWidget = ExpressionEditorWidget
  { _expwVariableBindings :: Value -- AppianList Dictionary
  , _expwHeight :: Text
  , _expwIndentOnTab :: Bool
  , _expwSaveInto :: [Text]
  , _expwSize :: Text
  , _expwEnableToolbar :: Bool
  , _expwDisableFormatting :: Bool
  , _expwValue :: Text
  , _expwCid :: Text
  , _expwTooltip :: Text
  , _expwShortcutsDocUri :: Text
  , _expwTestLabel :: Text
  , _expwReadOnly :: Bool
  , _expwRefreshAfter :: Text
  } deriving Show

data ExpressionInfoPanel = ExpressionInfoPanel
  { _editor :: ExpressionEditorWidget
  } deriving Show

data Dictionary = Dictionary
  { _dictValue :: AppianText
  , _dictUsageMetrics :: AppianList Text
  } deriving Show

data GridFieldIdent
  = IntIdent AppianInt
  | TxtIdent AppianString
  deriving Show

makePrisms ''GridFieldIdent

instance ToJSON GridFieldIdent where
  toJSON (IntIdent i) = toJSON i
  toJSON (TxtIdent i) = toJSON i

instance FromJSON GridFieldIdent where
  parseJSON v = IntIdent <$> parseJSON v
    <|> TxtIdent <$> parseJSON v

-- This needs to be updated to better reflect the actual SAIL component.
data GridFieldCell
  = TextCell (Vector (Maybe Text))
  | TextCellLink (Vector Text, Vector RecordRef)
  | TextCellDynLink (Vector Text, Vector DynamicLink)
  | ImageColumn (Vector ImageCell)
  | EmptyColumn
  deriving Show

newtype GridFieldColumn a
  = GridFieldColumn
    { _colRows :: Vector a
    }
  deriving (Show, Eq, Functor, Monoid, Foldable)

data ImageCell = ImageCell
  { _imgDocument :: CollaborationDocument
  , _imgOpaqueId :: Text
  , _imgCid :: Text
  } deriving Show

newtype CollaborationDocument = CollaborationDocument { _docId :: Int }
  deriving Show

data GridWidgetCell
  = GWTextField TextField
  | GWDecimal
  | GWDate DatePicker
  | GWDateTime
  | GWImage
    -- Can be a list of links
  | GWLink [AppianLink]
  | GWProgressBar
  | GWRichText
  | GWDropdown DropdownField
  | GWCheckbox CheckboxField
  deriving Show

data AppianLink
  = DocumentLink
  | ProcessTaskLink
  | RecordLink
    { _recordLinkLabel :: Text
    , _recordLinkRef :: RecordRef
    }
  | SafeLink
  | UserRecordLink
  | NewsEntryLink
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
makeLenses ''PagingInfo
makeLenses ''SortField
makeLenses ''GridSelection
makeLenses ''RadioButtonField
makeLenses ''ImageCell
makeLenses ''CollaborationDocument
makeLenses ''CheckboxField
makeLenses ''GridFieldColumn
makeLenses ''Identifier
makeLenses ''ExpressionEditorWidget
makeLenses ''ExpressionInfoPanel
makeLenses ''Dictionary
makeLenses ''AppianLink

makePrisms ''GridFieldCell
makePrisms ''GridValue
makePrisms ''GridField
makePrisms ''Identifier
makePrisms ''GridWidgetCell
makePrisms ''AppianLink

instance FromJSON DropdownField where
  parseJSON val@(Object o) = parseAppianTypeWith "DropdownField" (\typ -> isSuffixOf "DropdownField" typ || isSuffixOf "DropdownWidget" typ)  mkField val
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
        <*> o .:? "inlineLabel"
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
        <*> o .:? "disabled"
        <*> o .:? "accessibilityLabel"
        <*> o .: "_cId"
        <*> o .:? "keyboard"
        <*> o .: "align"
        <*> o .: "choices"
        <*> o .: "choiceLayout"
        <*> o .:? "value"
  parseJSON _ = fail "Could not parse CheckboxGroup"

instance FromJSON RadioButtonField where
  parseJSON val@(Object o) = parseAppianType "RadioButtonField" mkField val
    where
      mkField = RadioButtonField
        <$> o .: "saveInto"
        <*> o .: "_cId"
        <*> o .:? "required"
        <*> o .: "choices"
        <*> o .: "choiceLayout"
        <*> o .:? "value"

instance ToJSON RadioButtonField where
  toJSON rdg = object
    [ "saveInto" .= (rdg ^. rdgSaveInto)
    , "_cId" .= (rdg ^. rdgCid)
    , "required" .= (rdg ^. rdgRequired)
    , ("#t", "RadioButtonGroup")
    , "choices" .= (rdg ^. rdgChoices)
    , "choiceLayout" .= (rdg ^. rdgChoiceLayout)
    ]

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
    , "value" .= (cbg ^. cbgValue)
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

instance ToJSON GridValue where
  toJSON (Selectable gsl) = object
    [ "selected" .= (gsl ^. gslSelected)
    , ("#t", "GridSelection")
    , "pagingInfo" .= (gsl ^. gslPagingInfo)
    ]
  toJSON (NonSelectable pi) = _Object . at "#t" .~ (Just $ String "PagingInfo") $ toJSON pi

instance ToJSON PagingInfo where
  toJSON pgI = object
    [ "batchSize" .= (pgI ^. pgIBatchSize)
    , "sort" .= (pgI ^. pgISort)
    , "startIndex" .= (pgI ^. pgIStartIndex)
    ]

instance ToJSON SortField where
  toJSON sfd = object
    [ "field" .= (sfd ^. sfdField)
    , "ascending" .= (sfd ^. sfdAscending)
    ]

instance ToJSON CheckboxField where
  toJSON cbf = object
    [ "saveInto" .= (cbf ^. cbfSaveInto)
    , "_cId" .= (cbf ^. cbfCid)
    , ("#t", "CheckboxField")
    , "align" .= (cbf ^. cbfAlign)
    , "choices" .= (cbf ^. cbfChoices)
    , "choiceLayout" .= (cbf ^. cbfChoiceLayout)
    ]

instance FromJSON CheckboxField where
  parseJSON val@(Object o) = parseAppianType "CheckboxField" mkField val
    where
      mkField = CheckboxField
        <$> o .: "saveInto"
        <*> o .: "_cId"
        <*> o .: "align"
        <*> o .: "choices"
        <*> o .: "choiceLayout"
        <*> o .:? "value"

instance ToUpdate CheckboxField where
  toUpdate cbf = Update $ object
    [ "saveInto" .= (cbf ^. cbfSaveInto)
    , "value" .= (AppianList <$> cbf ^. cbfValue)
    , ("saveType", "PRIMARY")
    , "_cId" .= (cbf ^. cbfCid)
    , "model" .= cbf
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
  toJSON ui = object (
    [ "context" .= (ui ^. uiContext)
    , "uuid" .= (ui ^. uiUuid)
    , ("#t", "UiConfig")
    , "updates" .= (ui ^. uiUpdates)
    ] <> maybe mempty (\ident -> ["identifier" .= ident]) (ui ^. uiIdentifier)
    )

instance ToJSON Identifier where
  toJSON idf@(Identifier _ _ _ _ _) = object
    [ "siteUrlStub" .= (idf ^. idfSiteUrlStub)
    , "urlStub" .= (idf ^. idfUrlStub)
    , ("#t", "RecordInstanceListIdentifier")
    , "viewData" .= (idf ^. idfViewData)
    , "view" .= (idf ^. idfView)
    , "pageUrlStub" .= (idf ^. idfPageUrlStub)
    ]
  toJSON idf@(DesignObject _) = object
    [ ("#t", "DesignObject")
    , "opaqueId" .= (idf ^. objOpaqueId)
    ]

instance ToJSON ImageCell where
  toJSON img = object
    [ "_opaqueId" .= (img ^. imgOpaqueId)
    , "_cId" .= (img ^. imgCid)
    , "document" .= (img ^. imgDocument)
    ]

instance ToJSON CollaborationDocument where
  toJSON doc = object
    [ "id" .= (doc ^. docId)
    , ("#t", "DocumentImage")
    ]

instance ToJSON ExpressionEditorWidget where
  toJSON expw = object
    [ "variableBindings" .= (expw ^. expwVariableBindings)
    , "height" .= (expw ^. expwHeight)
    , "indentOnTab" .= (expw ^. expwIndentOnTab)
    , "saveInto" .= (expw ^. expwSaveInto)
    , "size" .= (expw ^. expwSize)
    , "enableToolbar" .= (expw ^. expwEnableToolbar)
    , "disableFormatting" .= (expw ^. expwDisableFormatting)
    , "value" .= (expw ^. expwValue)
    , "_cId" .= (expw ^. expwCid)
    , ("#t", "ExpressionEditorWidget")
    , "tooltip" .= (expw ^. expwTooltip)
    , "shortcutsDocUri" .= (expw ^. expwShortcutsDocUri)
    , "testLabel" .= (expw ^. expwTestLabel)
    , "readOnly" .= (expw ^. expwReadOnly)
    , "refreshAfter" .= (expw ^. expwRefreshAfter)
    ]

instance ToJSON ExpressionInfoPanel where
  toJSON panel = object
    [ "editor" .= (panel ^. editor)
    ]

instance ToJSON Dictionary where
  toJSON dict = object
    [ ("#t", "Dictionary")
    , ("#v", object
        [ "value" .= (dict ^. dictValue)
        , "usageMetricsKeys" .= (dict ^. dictUsageMetrics)
        ]
      )
    ]

instance FromJSON CollaborationDocument where
  parseJSON val@(Object o) = parseAppianType "CollaborationDocument" mkDoc val
    where
      mkDoc = CollaborationDocument <$> o .: "id"

instance FromJSON ImageCell where
  parseJSON val@(Object o) = parseAppianType "DocumentImage" mkCell val
    where
      mkCell = ImageCell
        <$> o .: "document"
        <*> o .: "_opaqueId"
        <*> o .: "_cId"
  parseJSON _ = fail $ "Could not parse DocumentImage"

instance FromJSON a => FromJSON (UiConfig a) where
  parseJSON val@(Object o) = parseAppianType "UiConfig" mkConfig val
    where
      mkConfig = UiConfig
        <$> o .: "context"
        <*> o .: "uuid"
        <*> o .:? "updates"
        <*> o .:? "identifier"
  parseJSON _ = fail "Could not parse UiConfig"

instance FromJSON Identifier where
  parseJSON val@(Object o) = parseAppianType "RecordInstanceListIdentifier" mkIdent val
    <|> parseAppianType "DesignObject" mkDesignObj val
    where
      mkIdent = Identifier
        <$> o .: "siteUrlStub"
        <*> o .: "urlStub"
        <*> o .: "viewData"
        <*> o .: "view"
        <*> o .: "pageUrlStub"
      mkDesignObj = DesignObject
        <$> o .: "opaqueId"

instance FromJSON TextField where
  parseJSON val@(Object o) = parseAppianTypeWith "TextField" (\t -> isSuffixOf "TextField" t || isSuffixOf "TextWidget" t) mkField val
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
        <*> o .:? "readOnly"

instance FromJSON PickerWidget where
  parseJSON val@(Object o) = parseAppianTypeWith "PickerWidget" (\t -> isSuffixOf "PickerWidget" t || isSuffixOf "PickerField" t) mkWidget val
    where
      mkWidget = PickerWidget
        <$> o .:? "maxSelections"
        <*> o .:? "selectedTokensHint"
        <*> o .:? "imageSize"
        <*> o .:? "numSuggestionsHint"
        <*> o .: "saveInto"
        <*> o .:? "noResultsLabel"
        <*> o .:? "searchingLabel"
        <*> o .:? "value"
        <*> o .:? "width"
        <*> o .: "_cId"
        <*> o .:? "required"
        <*> o .: "failedRequiredness"
        <*> o .:? "suggestions"
        <*> o .:? "testLabel"
        <*> o .: "placeholder"

instance FromJSON ParagraphField where
  parseJSON val@(Object o) = parseAppianTypeWith "ParagraphField" (isSuffixOf "ParagraphField") mkWidget val
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
        <*> o .:? "required"
        <*> o .: "align"
        <*> o .: "placeholder"
        <*> o .: "todayLabel"

instance FromJSON ExpressionEditorWidget where
  parseJSON val@(Object o) = ExpressionEditorWidget
        <$> o .:? "variableBindings" .!= (fromMaybe $ decode "{\"ri\": {\"#v\": [], \"#t\": \"Dictionary?list\"}}")
        <*> o .:? "height" .!= "FIT"
        <*> (readIndent o <|> o .: "indentOnTab")
        <*> o .: "saveInto"
        <*> o .: "size"
        <*> o .:? "enableToolbar" .!= True
        <*> o .:? "disableFormatting" .!= False
        <*> o .: "value"
        <*> o .: "_cId"
        <*> o .: "tooltip"
        <*> o .:? "shortcutsDocUri" .!= "https://docs.appian.com/suite/help/17.2/Expressions.html#keyboard-shortcuts"
        <*> o .: "testLabel"
        <*> o .: "readOnly"
        <*> o .: "refreshAfter"
    where
      fromMaybe (Just v) = v
      readIndent o = do
        txt <- o .: "indentOnTab"
        case (txt :: Text) of
          "true" -> return True
          "false" -> return False
          _ -> fail $ "Could not read indentOnTab value: " <> show txt

instance FromJSON ExpressionInfoPanel where
  parseJSON val@(Object o) = parseAppianTypeWith "ExpressionInfoPanel" (\typ -> isSuffixOf "ExpressionInfoPanel" typ) mkExprInfoPanel val
    where
      mkExprInfoPanel = ExpressionInfoPanel
        <$> (   o .: "editor"
            <|> o .: "editorWidget"
            )

-- instance FromJSON Dictionary where
  

instance FromJSON DynamicLink where
  parseJSON val@(Object o) = parseAppianType "DynamicLink" mkDynLink val
    where
      mkDynLink = DynamicLink
        <$> o .: "saveInto"
        <*> o .: "value"
        <*> o .:? "action"
        <*> o .: "_cId"
        <*> o .: "tooltip"
        <*> o .:? "confirmButtonStyle"
        <*> o .:? "validate"
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

instance FromJSON GridValue where
  parseJSON val@(Object o) = parseSelection <|> parsePaging
    where
      parseSelection = parseAppianType "GridSelection" mkField val
        where
          mkField = Selectable
            <$> (GridSelection
                 <$> o .: "selected"
                 <*> o .: "pagingInfo"
                )
      parsePaging = parseAppianType "PagingInfo" mkField val
        where
          mkField = NonSelectable <$> parseJSON val

instance FromJSON PagingInfo where
  parseJSON (Object o) = PagingInfo
    <$> o .: "batchSize"
    <*> o .:? "sort"
    <*> o .: "startIndex"

instance FromJSON SortField where
  parseJSON (Object o) = SortField
    <$> o .: "field"
    <*> o .:? "ascending"

instance FromJSON AppianLink where
  parseJSON val@(Object o) = parseRecordLink <|> pure DocumentLink
    where
      parseRecordLink = parseAppianType "RecordLink" mkRecordLink val
      mkRecordLink = RecordLink
        <$> o .: "label"
        <*> (RecordRef <$> o .: "_recordRef")

instance FromJSON GridWidgetCell where
  parseJSON val@(Object o) = parseGWLink
                     <|> parseGWTextField
                     <|> parseGWDate
                     <|> (GWDropdown <$> parseJSON val)
                     <|> (GWCheckbox <$> parseJSON val)
    where
      parseGWLink = GWLink
        <$> o .: "links"
      parseGWTextField = GWTextField <$> parseJSON val
      parseGWDate = GWDate <$> parseJSON val

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
    ]

instance ToUpdate DropdownField where
  toUpdate df = Update $ object
    [ "_cId" .= (df ^. dfCid)
    , "model" .= df
    , "value" .= (AppianInteger $ df ^. dfValue)
    , "saveInto" .= (df ^. dfSaveInto)
    , ("saveType", "PRIMARY")
    , "inlineLabel" .= (df ^. dfInlineLabel)
    ]

instance ToUpdate CheckboxGroup where
  toUpdate cbg = Update $ object
    [ "_cId" .= (cbg ^. cbgCid)
    , "saveInto" .= (cbg ^. cbgSaveInto)
    , "value" .= (AppianList <$> cbg ^. cbgValue)
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

instance ToUpdate (GridField a) where
  toUpdate gf = Update $ object
        [ "saveInto" .= (gf ^. gfSaveInto)
        , ("saveType", "PRIMARY")
        , "_cId" .= (gf ^. gfCid)
        , "model" .= (gf ^. gfModel)
        , "value" .= (gf ^. gfSelection)
        ]

instance ToUpdate RadioButtonField where
  toUpdate rdg = Update $ object
    [ "saveInto" .= (rdg ^. rdgSaveInto)
    , "value" .= (rdg ^. rdgValue)
    , ("saveType", "PRIMARY")
    , "_cId" .= (rdg ^. rdgCid)
    , "model" .= rdg
    ]

instance ToUpdate ExpressionEditorWidget where
  toUpdate expw = Update $ object
    [ "saveInto" .= (expw ^. expwSaveInto)
    , "value" .= (expw ^. expwValue . to mkDict)
    , ("saveType", "PRIMARY")
    , "_cId" .= (expw ^. expwCid)
    , "model" .= (expwValue .~ "" $ expw)
    ]
    where
      mkDict txt = Dictionary (AppianText txt) (AppianList [])
