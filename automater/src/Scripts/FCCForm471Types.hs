{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scripts.FCCForm471Types where

import Control.Lens
import ClassyPrelude
import qualified Data.Csv as Csv
import Appian.Instances

data Form471Conf = Form471Conf
  { _nFRNs :: Int
  , _nLineItems :: Int
  , _spin :: Text
  , _category :: Category
  , _applicant :: Login
  , _lineItemSize :: LineItemSize
  , _form470Search :: Form470SearchType
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

data Form470SearchType
  = ByBEN Text
  | By470 Text
  deriving Show

instance Csv.FromNamedRecord Form470SearchType where
  parseNamedRecord r =
    ByBEN <$> r Csv..: "BEN to Copy 470"
    <|> By470 <$> r Csv..: "470"

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

-- nFRNs :: Functor f => (Int -> f Int) -> Form471Conf -> f Form471Conf
-- nFRNs = lens get update
--   where
--     get = _nFRNs
--     update conf v = conf { _nFRNs = v }

-- nLineItems :: Functor f => (Int -> f Int) -> Form471Conf -> f Form471Conf
-- nLineItems = lens get update
--   where
--     get = _nLineItems
--     update conf v = conf { _nLineItems = v }

-- spin :: Functor f => (Text -> f Text) -> Form471Conf -> f Form471Conf
-- spin = lens get update
--   where
--     get = _spin
--     update conf v = conf { _spin = v }

-- applicant :: Functor f => (Login -> f Login) -> Form471Conf -> f Form471Conf
-- applicant = lens get update
--   where
--     get = _applicant
--     update conf v = conf { _applicant = v }

-- lineItemSize :: Functor f => (LineItemSize -> f LineItemSize) -> Form471Conf -> f Form471Conf
-- lineItemSize = lens get update
--   where
--     get = _lineItemSize
--     update conf v = conf { _lineItemSize = v }

-- category :: Functor f => (Category -> f Category) -> Form471Conf -> f Form471Conf
-- category = lens get update
--   where
--     get = _category
--     update conf v = conf { _category = v }

-- ben :: Functor f => (Text -> f Text) -> Form471Conf -> f Form471Conf
-- ben = lens get update
--   where
--     get = _ben
--     update conf v = conf { _ben = v }

makeLenses ''Form471Conf
makePrisms ''Form470SearchType
makePrisms ''Category
makePrisms ''LineItemSize
