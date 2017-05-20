{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Stats.ParseSample where

import ClassyPrelude hiding (throwM)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.Attoparsec.Types as A
import Data.Time hiding (readTime)
import Control.Lens hiding (index)
import Data.Time.Clock.POSIX
import Stats.Types

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

