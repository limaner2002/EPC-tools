{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Common where

import ClassyPrelude
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.State hiding (mapM_)

-- import Options.Applicative hiding ((<>))
-- import qualified Options.Applicative as OA
import Options.Applicative.Types

type Row a = [a]

class FromRow f a b where
  fromRow :: (MonadThrow m, Functor f) => f a -> m b

class ToUTCTime a where
  toUTCTime :: a -> UTCTime

newtype InvalidCSVRow = InvalidCSVRow Text
  deriving Show

instance Exception InvalidCSVRow

newtype JMeterTimeStamp = JMeterTimeStamp UTCTime
  deriving Show

newtype InvalidCSVEntry = InvalidCSVEntry Text
  deriving Show

instance Exception InvalidCSVEntry

data FilterState
  = Init
  | Between
  | GatherAllLast UTCTime
  | Done
  deriving Show

filterTime
  :: (MonadState FilterState m, ToUTCTime a) =>
     NominalDiffTime
     -> JMeterTimeStamp
     -> JMeterTimeStamp
     -> Either t a
     -> m Bool
filterTime logTimeDiff start end (Left _) = return False
filterTime logTimeDiff start end (Right v) = filterTime_ logTimeDiff start end v

                                             -- Stateful filter to get timestamps that occurred after the endtime in the log file
filterTime_ :: (MonadState FilterState m, ToUTCTime a) => NominalDiffTime -> JMeterTimeStamp -> JMeterTimeStamp -> a -> m Bool
filterTime_ logTimeDiff start@(JMeterTimeStamp startTime) end@(JMeterTimeStamp endTime) exprDetails = do
  filterState <- get
  case filterState of
    Init -> case isBetween start end exprDetails of
      False -> case endTime < (toUTCTime exprDetails) of
        False -> return False
        True -> put (GatherAllLast $ toUTCTime exprDetails) >> return True
      True -> put Between >> return True
    Between -> case isBetween start end exprDetails of
      True -> return True
      False -> put (GatherAllLast $ toUTCTime exprDetails) >> return True
    GatherAllLast ts ->
      case diffUTCTime (toUTCTime exprDetails) ts > logTimeDiff of
        False -> return True
        True -> put Done >> return False
    Done -> return False

isBetween_ :: ToUTCTime a => JMeterTimeStamp
     -> JMeterTimeStamp -> Either t a -> Bool
isBetween_ start end (Left _) = False
isBetween_ start end (Right v) = isBetween start end v

isBetween
  :: ToUTCTime a => JMeterTimeStamp -> JMeterTimeStamp -> a -> Bool
isBetween (JMeterTimeStamp startTime) (JMeterTimeStamp endTime) exprDetails
  = startTime <= time && time <= endTime
  where
    time = toUTCTime exprDetails

readJMeterTimeStamp ts = JMeterTimeStamp . posixSecondsToUTCTime <$> (\x -> fromIntegral x / 1000) <$> (readThrow ts)

parseJMeterTimeStamp :: ReadM JMeterTimeStamp
parseJMeterTimeStamp = do
  input <- readerAsk
  let mTs = JMeterTimeStamp . posixSecondsToUTCTime <$> (\x -> fromIntegral x / 1000) <$> (readMay input)
  case mTs of
    Nothing -> readerError $ show input <> " does not appear to be a valid JMeterTimeStamp."
    Just ts -> return ts

readThrow entry =
  case readMay entry of
    Nothing -> throwM $ InvalidCSVEntry $ "Could not read value " <> entry
    Just v -> return v
