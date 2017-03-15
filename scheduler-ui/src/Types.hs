{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( JobStatus (..)
  , Job (..)
  , JobQueue
  ) where

import ClassyPrelude

data JobStatus
  = Queued
  | Running
  | Finished
  deriving (Show, Generic)

data Job = Job
  { jobName :: Text
  , jobStatus :: JobStatus
  } deriving (Show, Generic)

type JobQueue = Map Text Job
