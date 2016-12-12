{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import ClassyPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Path

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

data UnValidated
data Validated
