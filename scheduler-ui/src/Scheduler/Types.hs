{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler.Types
  ( JobStatus (..)
  , Job-- (..)
  , newJob
  , JobQueue
  , jobName
  , jobStatus
  , jobVal
  , jobId
  , qJobs
  , qStartTime
  , emptyQueue
  , qStatus
  , QueueException (..)
  , QStatus (..)
  ) where

import Control.Lens
import GHC.Exception
import Prelude
import Data.Text (Text)
import GHC.Generics
import Data.Time
import Data.Aeson
import Control.Monad.State

data JobStatus
  = Queued
  | Running
  | Finished
  | Cancelling
  | Cancelled
  deriving (Show, Eq, Generic)

data Job a = Job
  { _jobNameInternal :: Text
  , _jobStatus :: JobStatus
  , _jobValInternal :: a
  , _jobIdInternal :: Int
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

newJob :: MonadState Int m => Text -> JobStatus -> a -> m (Job a)
newJob name status jobVal = state newJob'
  where
    newJob' id = (Job name status jobVal id, id+1)

jobId :: (Functor f, Contravariant f) => LensLike' f (Job a) Int
jobId = getting jobIdInternal

jobName :: (Functor f, Contravariant f) => LensLike' f (Job a) Text
jobName = getting jobNameInternal

jobVal :: (Functor f, Contravariant f) => LensLike' f (Job a) a
jobVal = getting jobValInternal
