{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scripts.FCCForm471Types where

import Control.Lens
import ClassyPrelude
import qualified Data.Csv as Csv
import Appian.Instances
import Scripts.Common (HasLogin (..), SelectOrgMethod (..))
import qualified Data.Attoparsec.Text as T

data Form471Conf = Form471Conf
  { _nFRNs :: Int
  , _nLineItems :: Int
  , _spin :: Text
  , _category :: Category
  , _applicant :: Login
  , _lineItemSize :: LineItemSize
  , _form470Search :: Form470SearchType
  , _createFRNType :: CreateFRNType
  , _selectOrgMethod :: SelectOrgMethod
  } deriving Show

instance Csv.FromNamedRecord Form471Conf where
  parseNamedRecord r = Form471Conf
    <$> r Csv..: "nFRNs"
    <*> r Csv..: "nLineItems"
    <*> r Csv..: "spin"
    <*> r Csv..: "category"
    <*> Csv.parseNamedRecord r
    <*> r Csv..: "lineItemSize"
    <*> Csv.parseNamedRecord r
    <*> r Csv..: "Create FRN Mode"
    <*> r Csv..: "Select Org Method"

data Form470SearchType
  = ByBEN Text
  | By470 Text
  | No470
  deriving Show

instance Csv.FromNamedRecord Form470SearchType where
  parseNamedRecord r =
    ByBEN <$> r Csv..: "BEN to Copy 470"
    <|> By470 <$> r Csv..: "470"
    <|> pure No470

instance Csv.ToNamedRecord Form470SearchType where
  toNamedRecord (ByBEN t) = Csv.namedRecord
    [ ("BEN to Copy 470", encodeUtf8 t)
    ]
  toNamedRecord (By470 t) = Csv.namedRecord
    [ ("470", encodeUtf8 t)
    ]
  toNamedRecord No470 = mempty

data Form470FunctionType
  = BMIC
  deriving (Show, Eq)

data LineItemSize
  = Small
  | Regular
  deriving (Show, Read)

data Category
  = Cat1
  | Cat2

instance Show Category where
  show Cat1 = "Category 1"
  show Cat2 = "Category 2"

instance Csv.FromField LineItemSize where
  parseField bs = case readMay (decodeUtf8 bs) of
    Nothing -> fail $ show bs <> " does not appear to be a valid Line Item Size. Use only 'Small' or 'Regular'"
    Just size -> return size

instance Csv.FromField Category where
  parseField bs = case decodeUtf8 bs of
    "1" -> return Cat1
    "2" -> return Cat2
    _ -> fail $ show bs <> " does not appear to be a valid category. Use only '1' or '2'"

data CreateFRNType
  = NewFRN
  | CopyFRN SearchFRNMethod
  deriving (Show, Eq)

instance Csv.FromField CreateFRNType where
  parseField bs = either fail pure $ parseResult
    where
      parseResult = T.parseOnly parseCreateFRNType $ decodeUtf8 bs
      parseCreateFRNType
        = T.string "New" *> pure NewFRN
        <|> T.string "Copy " *> (CopyFRN <$> parseFRNMethod)

data SearchFRNMethod
  = ByFCCForm471 Int
  | ByFRNNumber Int
  deriving (Show, Eq)

instance Csv.FromField SearchFRNMethod where
  parseField bs = either fail pure $ parseResult
    where
      parseResult = T.parseOnly parseFRNMethod $ decodeUtf8 bs

parseFRNMethod
  = T.string "471 " *> (ByFCCForm471 <$> T.decimal)
  <|> T.string "FRN " *> (ByFRNNumber <$> T.decimal)
  <|> fail "Could not decode SearchFRNMethod"

makeLenses ''Form471Conf
makePrisms ''Form470SearchType
makePrisms ''Category
makePrisms ''LineItemSize
makePrisms ''CreateFRNType

instance HasLogin Form471Conf where
  getLogin conf = conf ^. applicant

instance Csv.ToNamedRecord Form471Conf where
  toNamedRecord conf = Csv.namedRecord
    [ ("nFRNs", conf ^. nFRNs . to (encodeUtf8 . tshow))
    , ("nLineItems", conf ^. nLineItems . to (encodeUtf8 . tshow))
    , ("spin", conf ^. spin . to encodeUtf8)
    , ("category", conf ^. category . to (encodeUtf8 . tshow))
    , ("username", conf ^. applicant . username . to encodeUtf8)
    , ("password", conf ^. applicant . password . to encodeUtf8)
    , ("lineItemSize", conf ^. lineItemSize . to (encodeUtf8 . tshow))
    ]
    <> Csv.toNamedRecord (conf ^. form470Search)
