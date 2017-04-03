{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler where

import Scheduler.Types
import ClassyPrelude
import Control.Lens

data QueueException = QueueException Text
  deriving (Show, Eq)

instance Exception QueueException

incJobStatus :: JobStatus -> JobStatus
incJobStatus Queued = Running
incJobStatus Running = Finished
incJobStatus Finished = Finished

getNotFinished :: MonadThrow m => JobQueue a -> m (Job a, JobQueue a)
getNotFinished q = (,) <$> job <*> (mappend <$> pure done <*> rest'')
  where
    (done, rest) = (takeWhile isFinished q, dropWhile isFinished q)
    (job, rest') = (headThrow rest, drop 1 rest)
    rest'' = (:) <$> job' <*> pure rest'
    job' = fmap (jobStatus .~ Running) $ job

checkRunning :: MonadThrow m => Job a -> m (Job a)
checkRunning job = case isRunning job of
                     True -> throwM $ QueueException "There is a job still running."
                     False -> pure job
    
isQueued :: Job a -> Bool
isQueued j = j ^. jobStatus == Queued

isFinished :: Job a -> Bool
isFinished j = j ^. jobStatus == Finished

isRunning :: Job a -> Bool
isRunning j = j ^. jobStatus == Running

q1 = [Job "6742" Queued Nothing, Job "6619" Queued Nothing]

q2 = [Job "3761" Running Nothing, Job "6781" Queued Nothing]

q3 = [Job "4222" Finished Nothing, Job "4313" Queued Nothing]

data EmptyHeadException = EmptyHeadException Text
  deriving (Show, Eq)

instance Exception EmptyHeadException

headThrow :: (MonadThrow m, MonoFoldable mono) => mono -> m (Element mono)
headThrow l = case headMay l of
            Nothing -> throwM $ EmptyHeadException "headT: empty"
            Just v -> return v

addJob :: Job a -> JobQueue a -> JobQueue a
addJob = (:)

getQueue :: MonadBase IO m => TVar (JobQueue a) -> m (JobQueue a)
getQueue = liftBase . atomically . readTVar

setQueue :: MonadBase IO m => TVar (JobQueue a) -> JobQueue a -> m ()
setQueue var q = liftBase $ atomically $ writeTVar var q

setJob :: Job a -> JobQueue a -> JobQueue a
setJob job q = first <> [job] <> drop 1 rest
  where
    (first, rest) = splitWhenFirst (\j -> j ^. jobName == job ^. jobName) q

splitWhenFirst :: IsSequence seq => (Element seq -> Bool) -> seq -> (seq, seq)
splitWhenFirst f s = (takeWhile (not . f) s, dropWhile (not . f) s)

getFirst :: (MonadThrow m, IsSequence seq) => (Element seq -> Bool) -> seq -> m (Element seq, seq, seq)
getFirst f s = (,,) <$> a <*> pure first <*> pure rest'
  where
    (first, rest) = splitWhenFirst f s
    (a, rest') = (headThrow rest, drop 1 rest)
