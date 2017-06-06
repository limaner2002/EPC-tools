{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module CreateRequest where

import ClassyPrelude hiding (filter)
import Data.Aeson

import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ListArrow (runLA)
import JSONTreeLA

import Data.Default
import Control.Arrow

data Action a
  = HRef
    { href :: Text
    , hrMethod :: Text
    , hrBody :: ByteString
    , hrAccept :: Maybe ByteString
    }
  | Update
    { saveInto :: [SaveInto]
    , value :: Value_ a
    , atomic :: Bool
    , upTyp :: Text
    , taskId :: Int
    }
  | Next (Action a)
  deriving Show

instance ToJSON a => ToJSON (Action a) where
  toJSON (Update s v at typ _) = object
    [ "#v" .= s
    , "value" .= v
    , "atomic" .= at
    , "#t" .= typ
    ]
  toJSON (HRef t _ _ _) = object
    [ "link" .= t
    ]

data Value_ a = Value_
  { val :: a
  , typ :: Text
  } deriving Show

instance ToJSON a => ToJSON (Value_ a) where
  toJSON (Value_ v typ) = object
    [ "#v" .= v
    , "#t" .= typ
    ]

newtype SaveInto = SaveInto Text
  deriving Show

instance ToJSON SaveInto where
  toJSON (SaveInto t) = toJSON t

instance Default Text where     -- This is a quick hack. Better would be to use a newtype wrapper
  def = mempty

instance Default a => Default (Value_ a) where
  def = Value_ def mempty

instance Default a => Default (Action a) where
  def = Update def def False "SaveRequest?list" 0

valToTextMay :: Value -> Maybe Text
valToTextMay (String t) = Just t
valToTextMay _ = Nothing

baseUrl :: IsString t => t
baseUrl = "https://portal-test4.appiancloud.com"
