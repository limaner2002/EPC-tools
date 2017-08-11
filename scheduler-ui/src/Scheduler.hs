{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler where

import Scheduler.Types
import ClassyPrelude
import Control.Lens hiding (Index)

incJobStatus :: JobStatus -> JobStatus
incJobStatus Queued = Running
incJobStatus Running = Finished
incJobStatus Finished = Finished
incJobStatus Cancelled = Cancelled

getNotFinished :: MonadThrow m => JobQueue a -> m (Job a, JobQueue a)
getNotFinished q = (,) <$> job <*> (fmap (qJobs .~) newList <*> pure q)
  where
    (done, rest) = span isFinished jobList
    (job, rest') = (headThrow rest, drop 1 rest)
    rest'' = (:) <$> job' <*> pure rest'
    job' = (jobStatus .~ Running) <$> job
    jobList = q ^. qJobs
    newList = mappend <$> pure done <*> rest''

checkRunning :: MonadThrow m => Job a -> m (Job a)
checkRunning job = case isRunning job of
                     True -> throwM $ QueueException "There is a job still running."
                     False -> pure job
    
checkQueueRunning :: MonadThrow m => JobQueue a -> m (JobQueue a)
checkQueueRunning q =
  case q ^. qStatus of
    QRunning -> throwM $ QueueException "The queue is already running."
    QWaiting -> pure q

isQueued :: Job a -> Bool
isQueued j = j ^. jobStatus == Queued

isFinished :: Job a -> Bool
isFinished j = j ^. jobStatus == Finished

isRunning :: Job a -> Bool
isRunning j = j ^. jobStatus == Running

newtype EmptyHeadException = EmptyHeadException Text
  deriving (Show, Eq)

instance Exception EmptyHeadException

         -- This is a bad way of doing this. I should refactor this
         -- possibly using an Either type to indicate that there are
         -- no jobs queued and leave the exception handling to the
         -- actual exceptions.
headThrow :: (MonadThrow m, MonoFoldable mono) => mono -> m (Element mono)
headThrow l = case headMay l of
            Nothing -> throwM $ EmptyHeadException "There are no jobs queued. Not doing anything."
            Just v -> return v

-- Appends a job to the end of the queue
addJob :: Job a -> JobQueue a -> JobQueue a
addJob job = qJobs %~ (|> job)

getQueue :: MonadBase IO m => TVar (JobQueue a) -> m (JobQueue a)
getQueue = liftBase . atomically . readTVar

getQueue' :: TVar (JobQueue a) -> STM (JobQueue a)
getQueue' = readTVar

setQueue :: MonadBase IO m => TVar (JobQueue a) -> JobQueue a -> m ()
setQueue var q = liftBase $ atomically $ writeTVar var q

setQueue' :: TVar (JobQueue a) -> JobQueue a -> STM ()
setQueue' var q = writeTVar var q

setJob :: Job a -> JobQueue a -> JobQueue a
setJob job q = qJobs .~ (firstPart <> [job] <> drop 1 rest) $ q
  where
    (firstPart, rest) = splitWhenFirst (\j -> j ^. jobName == job ^. jobName) (q ^. qJobs)

splitWhenFirst :: IsSequence seq => (Element seq -> Bool) -> seq -> (seq, seq)
splitWhenFirst f s = span (not . f) s

getFirst :: (MonadThrow m, IsSequence seq) => (Element seq -> Bool) -> seq -> m (Element seq, seq, seq)
getFirst f s = (,,) <$> a <*> pure firstPart <*> pure rest'
  where
    (firstPart, rest) = splitWhenFirst f s
    (a, rest') = (headThrow rest, drop 1 rest)

schedule :: MonadBase IO m => TVar (JobQueue a) -> m () -> m ()
schedule tScheduledTime action = loop
  where
    loop = do
      mScheduledTime <- liftBase $ atomically $ readTVar tScheduledTime
      case mScheduledTime ^. qStartTime of
        Nothing -> do
          liftBase $ putStrLn "The job has been cancelled."
          return ()
        Just scheduledTime -> do
          ct <- liftBase getCurrentTime
          case ct >= scheduledTime of
            True -> do
              liftBase $ putStrLn "Running job now"
              action
              liftBase $ atomically $ modifyTVar tScheduledTime (qStartTime .~ Nothing)
            False -> do
              liftBase $ threadDelay 1000000
              loop

    -- Utility Functions

splitEm :: IsSequence seq => Index seq -> seq -> (seq, (seq, (seq, seq)))
splitEm n l = (fmap . fmap) (splitAt 1) $ fmap (splitAt 1) $ splitAt n l

moveBack :: (Semigroup t, IsSequence t) => Index t -> t -> t
moveBack n l = pfx <> b <> a <> sfx
  where
    (pfx, (a, (b, sfx))) = splitEm n l

moveUp :: (IsSequence t, Semigroup t) => Index t -> t -> t
moveUp n l = moveBack (n-1) l

remove :: Foldable f => Int -> f (Job a) -> [Job a]
remove n l = l ^.. folded . ifiltered (\i job -> i /= n || (job ^. jobStatus == Running))

             -- Finds the next queued job, marks it as running and returns the job.
setRunning :: TVar (JobQueue a) -> STM (Job a)
setRunning var = do
  q <- readTVar var
  (job, q') <- getNotFinished q
  let job' = jobStatus .~ Running $ job
      q'' = setJob job' q'
  writeTVar var q''
  return job'

setFinished :: TVar (JobQueue a) -> Job a -> STM ()
setFinished var job = modifyTVar var setFinished'
  where
    setFinished' q = setJob finishedJob q
    finishedJob = jobStatus .~ Finished $ job
