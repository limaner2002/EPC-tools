{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Scheduler.Types
  ( JobStatus (..)
  , Job (..)
  , JobQueue
  , jobName
  , jobStatus
  , qJobs
  , qStartTime
  , emptyQueue
  , qStatus
  , QueueException (..)
  , QStatus (..)
  , testJobQueue
  ) where

import Control.Lens
import GHC.Exception
import Prelude
import Data.Text (Text)
import GHC.Generics
import Data.Time
import Data.Aeson

data JobStatus
  = Queued
  | Running
  | Finished
  | Cancelled
  deriving (Show, Eq, Generic)

data Job a = Job
  { _jobName :: Text
  , _jobStatus :: JobStatus
  , _jobVal :: a
  } deriving (Show, Generic)

data JobQueue a = JobQueue
  { _qJobs :: [Job a]
  , _qStartTime :: Maybe UTCTime
  , _qStatus :: QStatus
  } deriving (Generic, Show)

data QStatus
  = QRunning
  | QWaiting
  deriving (Show, Generic)

data QueueException = QueueException Text
  deriving (Show, Eq, Generic)

instance Exception QueueException

instance ToJSON QueueException
instance FromJSON QueueException

makeLenses ''Job
makeLenses ''JobQueue

emptyQueue :: JobQueue a
emptyQueue = JobQueue mempty Nothing QWaiting

instance ToJSON JobStatus
instance FromJSON JobStatus

instance ToJSON QStatus
instance FromJSON QStatus

instance ToJSON a => ToJSON (Job a)
instance FromJSON a => FromJSON (Job a)

instance ToJSON a => ToJSON (JobQueue a)
instance FromJSON a => FromJSON (JobQueue a)

testJobQueue :: JobQueue Text
testJobQueue = foldl (\jq job -> qJobs %~ (|> job) $ jq) emptyQueue testJobs

testJobs :: [Job Text]
testJobs = [ Job "Job 1" Queued "Tis my duty!"
           , Job "Job 2" Queued "Tis my duty!"
           , Job "471 Crap" Queued "Tis my duty!"
           , Job "Job Form 500 things" Queued "Tis my duty!"
           ]
