{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module ParseSystem where

import ClassyPrelude hiding (throwM)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.Attoparsec.Types as A
import StreamParser
import Data.Time hiding (readTime)
import Control.Lens hiding (index)

data System
  = LoadAverage
    { _timeStamp :: LocalTime
    , _val :: Double
    } deriving Show

makeLenses ''System

parseLoadAverage :: Int -> TimeZone -> CSVSettings -> A.Parser ByteString System
parseLoadAverage column tz csvSettings = do
  row <- parseRow csvSettings
  let mPair = (,) <$> index row 0 <*> index row column
  case mPair of
    Nothing -> fail "Could not find the load average"
    Just (timeStr, valStr) -> do
      let eVal = readTime tz (unpack (decodeUtf8 timeStr), unpack (decodeUtf8 valStr))
      case eVal of
        Left msg -> fail msg
        Right (time, val) -> return $ LoadAverage time val

readTime :: TimeZone -> (String, String) -> Either String (LocalTime, Double)
readTime tz (t, v) = do
  time <- parseTimeM True defaultTimeLocale "%e %b %Y %X %Z" t
  case readMay v of
    Nothing -> Left $ "Could not read " <> show v
    Just dbl -> Right $ (utcToLocalTime tz time, dbl)
