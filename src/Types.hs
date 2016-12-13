{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( LogSettings (..)
  , JMeterOpts (..)
  , RunName (..)
  , NUsers (..)
  , Run (..)
  , Validated
  , UnValidated
  , JobDelay (..)
  , fromRunName
  , ScheduledTime
  , mkScheduledTime
  , fromScheduledTime
  , ToScheduledTime (..)
  , scheduledTimeToLocalTime
  , SubmitStatus (..)
  ) where

import ClassyPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Path
import Data.Time

data LogSettings = LogSettings
  { username :: Text
  , password :: Text
  , nodeNames :: [Text]
  , logName :: Maybe (Path Rel File)
  , logDestination :: Maybe (Path Rel Dir)
  , logUrl :: Text
  } deriving (Show, Generic)

instance ToJSON LogSettings
instance FromJSON LogSettings

data SubmitStatus
  = Confirmation [Path Rel File]
  | Submitted
  | SubmissionError Text
  | SubmissionSuccess Text
  deriving (Show, Generic)

instance ToJSON SubmitStatus
instance FromJSON SubmitStatus

data JMeterOpts = JMeterOpts
  { nRuns :: Run
  , nUsers :: [NUsers]
  , jmxPath :: FilePath
  , jmeterPath :: FilePath
  , runName :: RunName
  , otherOpts :: [Text]
  , sleepTime :: JobDelay
  } deriving (Show, Generic)

instance ToJSON JMeterOpts
instance FromJSON JMeterOpts

newtype NUsers = NUsers Int
  deriving (Show, Eq, Generic, Read)

instance ToJSON NUsers
instance FromJSON NUsers

newtype Run = Run Int
  deriving (Show, Eq, Generic, Read)

instance ToJSON Run
instance FromJSON Run

newtype RunName = RunName Text
  deriving (Show, Generic, Read, Eq, Ord)

fromRunName :: RunName -> Text
fromRunName (RunName n) = n

instance ToJSON RunName
instance FromJSON RunName

data JobDelay
  = Delay Int -- The amount of time to sleep in seconds before executing the next test
  | AtTime ToScheduledTime -- The time at which a job should be executed
  deriving (Show, Generic, Read)

instance ToJSON JobDelay
instance FromJSON JobDelay

data UnValidated
data Validated

newtype ScheduledTime = ScheduledTime UTCTime
  deriving Show

data ToScheduledTime
 = At LocalTime
 | TOD TimeOfDay
 deriving (Show, Read, Generic)

instance ToJSON ToScheduledTime
instance FromJSON ToScheduledTime

mkScheduledTime :: TimeZone -> ToScheduledTime -> UTCTime -> ScheduledTime
mkScheduledTime tz (At t) _ = ScheduledTime $ localTimeToUTC tz t
mkScheduledTime tz (TOD localTimeOfDay) utcTime
  | localScheduledTime >= localTime = ScheduledTime $ localTimeToUTC tz localScheduledTime
  | otherwise = ScheduledTime $ localTimeToUTC tz nextDay
  where
    localTime = utcToLocalTime tz utcTime
    localDay' = localDay localTime
    localScheduledTime = LocalTime localDay' localTimeOfDay
    nextDay = LocalTime (addDays 1 localDay') localTimeOfDay

fromScheduledTime :: ScheduledTime -> UTCTime
fromScheduledTime (ScheduledTime t) = t

scheduledTimeToLocalTime :: TimeZone -> ScheduledTime -> LocalTime
scheduledTimeToLocalTime tz = utcToLocalTime tz . fromScheduledTime
