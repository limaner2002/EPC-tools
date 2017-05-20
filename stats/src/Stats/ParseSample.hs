{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Stats.ParseSample where

import ClassyPrelude hiding (throwM)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.Attoparsec.Types as A
import Data.Time hiding (readTime)
import Control.Lens hiding (index)
import Data.Time.Clock.POSIX

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

parseHTTPSample :: [Text] -> A.Parser a HTTPSample
parseHTTPSample row =
  case toHTTPSample row of
    Nothing -> fail ("Could not parse " <> show row <> ". It does not appear to be a valid HTTP sample.")
    Just sample -> return sample

toHTTPSample :: [Text] -> Maybe HTTPSample
toHTTPSample [ timeStamp, elapsed, label, responseCode, responseMessage
             , threadName, dataType, success, failureMessage
             , bytes, sentBytes, grpThreads, allThreads, latency
             , idleTime, connect
             ] = HTTPSample
             <$> (posixSecondsToUTCTime <$> (fromIntegral <$> (div <$> readMay timeStamp <*> pure 1000)))
             <*> readMay elapsed
             <*> pure (Label label)
             <*> toResponseCode responseCode
             <*> pure responseMessage
             <*> pure threadName
             <*> pure dataType
             <*> toBool success
             <*> pure failureMessage
             <*> readMay bytes
             <*> readMay sentBytes
             <*> readMay grpThreads
             <*> readMay allThreads
             <*> readMay latency
             <*> readMay idleTime
             <*> readMay connect
toHTTPSample _ = Nothing

readMay' :: (Element c ~ Char, MonoFoldable c, Read a) => c -> Maybe (Maybe a)
readMay' val
  | onull val = Just Nothing
  | otherwise = case readMay val of
                  Nothing -> Nothing
                  Just v -> Just $ Just v

toBool :: Text -> Maybe Bool
toBool "false" = Just False
toBool "true" = Just True
toBool _ = Nothing

toResponseCode :: Text -> Maybe ResponseCode
toResponseCode txt
  | onull txt = Just NoResponseCode
  | isPrefixOf "Non HTTP response code: " txt = Just $ NonHTTPResponseCode txt
  | otherwise = HTTPResponseCode <$> readMay txt

makeLenses ''HTTPSample
makeLenses ''Label
makePrisms ''ResponseCode
