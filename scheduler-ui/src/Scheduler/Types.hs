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
  ) where

import ClassyPrelude
import Control.Lens

data JobStatus
  = Queued
  | Running
  | Finished
  deriving (Show, Eq, Generic)

data Job a = Job
  { _jobName :: Text
  , _jobStatus :: JobStatus
  , _jobVal :: a
  } deriving (Show, Generic)

data JobQueue a = JobQueue
  { _qJobs :: [Job a]
  , _qStartTime :: Maybe UTCTime
  } deriving Generic

makeLenses ''Job
makeLenses ''JobQueue

emptyQueue = JobQueue mempty Nothing
