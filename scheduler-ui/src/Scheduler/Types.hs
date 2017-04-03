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

type JobQueue a = [Job a]

makeLenses ''Job
