{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Scheduler.Google.Types where

import Prelude hiding (lookup)
import Data.Text (Text)
import Data.Aeson
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Except
import Lucid
import Data.Semigroup
import GHC.Base ((<|>))
import Data.Containers (lookup)
import GHC.Generics (Generic)

data DriveFileType
  = File Text
  | Folder
  | Zip
  deriving (Show, Eq)

data DriveFile a = DriveFile
  { _fName :: Text
  , _fId :: Text
  , _fType :: a
  }
  | Root
  deriving (Show, Eq)

data DriveRequest a = DriveRequest
  { _reqFile :: a
  , _reqCrumbs :: BreadCrumbs a
  } deriving Show

newtype BreadCrumbs a = BreadCrumbs { _bcCrumbs :: [a] }
  deriving (Show, Monoid)

makeLenses ''DriveFile
makePrisms ''DriveFileType
makePrisms ''DriveFile
makeLenses ''DriveRequest

instance ToJSON DriveFileType where
  toJSON (File mt) = object
    [ "tag" .= ("File" :: Text)
    , "mimetype" .= mt
    ]
  toJSON Folder = object ["tag" .= ("Folder" :: Text)]
  toJSON Zip = object ["tag" .= ("Zip" :: Text)]

instance FromJSON DriveFileType where
  parseJSON (Object o) = parseFile <|> parseFolder <|> parseZip
    where
      parseFile = checkTag "File" o (File <$> o .: "mimetype")
      parseFolder = checkTag "Folder" o (pure Folder)
      parseZip = checkTag "Zip" o (pure Zip)

instance ToJSON a => ToJSON (DriveFile a) where
  toJSON Root = object [("tag", "Root")]
  toJSON f = object
    [ "fName" .= (f ^. fName)
    , "fId" .= (f ^. fId)
    , "fType" .= (f ^? fType)
    ]        

instance FromJSON a => FromJSON (DriveFile a) where
  parseJSON (Object o) = parseRoot <|> parseFile
    where
      parseRoot = checkTag "Root" o (pure Root)
      parseFile = DriveFile
        <$> o .: "fName"
        <*> o .: "fId"
        <*> o .: "fType"
  parseJSON _ = fail "Could not parse DriveFile"

instance ToJSON a => ToJSON (DriveRequest a) where
  toJSON req = object
    [ "reqFile" .= (req ^. reqFile)
    , "reqCrumbs" .= (req ^. reqCrumbs)
    ]

instance FromJSON a => FromJSON (DriveRequest a) where
  parseJSON (Object o) = DriveRequest
    <$> o .: "reqFile"
    <*> o .: "reqCrumbs"
  parseJSON _ = fail "Expecting an Object when decoding DriveRequest"

checkTag :: Monad m => Value -> Object -> m a -> m a
checkTag tag mp f = case lookup "tag" mp of
  Nothing -> fail $ "field \"tag\": " <> show tag <> " not present"
  Just t -> case tag == t of
    True -> f
    False -> fail $ "Expecting type of " <> show tag <> " but got " <> show t

instance ToHtml DriveFileType where
  toHtml (File mt) = "File" <> toHtml mt
  toHtml Folder = "Folder"
  toHtml Zip = "Zip"
  toHtmlRaw = toHtml

instance ToHtml a => ToHtml (DriveFile a) where
  toHtml Root = td_ $ toHtml ("Root" :: Text)
  toHtml f = do
    td_ $ f ^. fName . to toHtml
    td_ $ f ^. fType . to toHtml
  toHtmlRaw = toHtml

data Files
  = Files
    { _getFiles :: [DriveFile DriveFileType]
    }
  | Fetching
  deriving (Show, Generic)

makeLenses ''Files
makePrisms ''Files
makeLenses ''BreadCrumbs

instance ToJSON Files
instance FromJSON Files

breadCrumbs :: BreadCrumbs a
breadCrumbs = mempty

addCrumb :: a -> BreadCrumbs a -> BreadCrumbs a
addCrumb a = bcCrumbs %~ (|> a)

instance ToJSON a => ToJSON (BreadCrumbs a) where
  toJSON bc = object
    ["crumbs" .= (bc ^. bcCrumbs)]

instance FromJSON a => FromJSON (BreadCrumbs a) where
  parseJSON (Object o) = BreadCrumbs <$> o .: "crumbs"

type DFile = DriveFile DriveFileType

data NavResponse
  = FileList (Files, BreadCrumbs DFile)
  | AddJob (Text, Text)
  deriving (Show, Generic)

instance ToJSON NavResponse
instance FromJSON NavResponse

makePrisms ''NavResponse
