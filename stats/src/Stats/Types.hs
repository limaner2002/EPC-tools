{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Stats.Types where

import ClassyPrelude
import Control.Lens
import Data.TDigest

data Stat = Stat
  { _statTime :: UTCTime
  , _statTotal :: Int
  , _statErrors :: Int
  , _statElapsed :: Int
  , _statDigest :: TDigest 25
  , _statMin :: Infinite Int
  , _statMax :: Int
  } deriving Show

data Infinite a = Infinity | Only a
  deriving Show

instance Ord a => Ord (Infinite a) where
  Only _ <= Infinity = True
  Infinity <= Infinity = True
  Infinity <= Only _ = False
  Only a <= Only b = a <= b

instance Eq a => Eq (Infinite a) where
  Infinity == Infinity = True
  Only a == Only b = a == b
  _ == _ = False

dispInfinite :: Show a => Infinite a -> String
dispInfinite Infinity = "Infinity"
dispInfinite (Only n) = show n

type Dict = Map Label Stat

newStat :: UTCTime -> Stat
newStat ts = Stat ts 0 0 0 mempty Infinity 0

data HTTPSample = HTTPSample
  { _timeStamp :: UTCTime
  , _elapsed :: Int
  , _label :: Label
  , _responseCode :: ResponseCode
  , _responseMessage :: Text
  , _threadName :: Text
  , _dataType :: Text
  , _success :: Bool
  , _failureMessage :: Text
  , _bytes :: Int
  , _sentBytes :: Int
  , _grpThreads :: Int
  , _allThreads :: Int
  , _latency :: Int
  , _idleTime :: Int
  , _connect :: Int
  } deriving Show

newtype Label = Label {_labelVal :: Text}
  deriving (Show, Ord, Eq)

data ResponseCode
  = HTTPResponseCode Int
  | NonHTTPResponseCode Text
  | NoResponseCode
  deriving Show

makeLenses ''HTTPSample
makeLenses ''Label
makePrisms ''ResponseCode
makeLenses ''Stat

