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
incJobStatus Cancelling = Cancelled
incJobStatus Cancelled = Cancelled

getFirstJobNot :: (Job a -> Bool) -> (Job a -> Maybe (Job a)) -> JobQueue a -> Maybe (Job a, JobQueue a)
getFirstJobNot pred update q = (,) <$> job <*> (fmap (qJobs .~) newList <*> pure q)
  where
    (done, rest) = span pred jobList
    (job, rest') = (headMay rest, drop 1 rest)
    rest'' = (:) <$> job' <*> pure rest'
    job' =  join $ mapM update job
    jobList = q ^. qJobs
    newList = mappend <$> pure done <*> rest''

getNotFinished :: JobQueue a -> Maybe (Job a, JobQueue a)
getNotFinished = getFirstJobNot (\j -> isFinished j || isCancelled j) update
  where
    update job = case job ^. jobStatus == Queued of
      True -> Just (jobStatus .~ Running $ job)
      False -> Nothing

cancelRunning :: JobQueue a -> Maybe (Job a, JobQueue a)
cancelRunning = getFirstJobNot (not . isRunning) update
  where
    update job = case job ^. jobStatus of
      Running -> Just (jobStatus .~ Cancelling $ job)
      _ -> Nothing

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

isCancelling :: Job a -> Bool
isCancelling j = j ^. jobStatus == Cancelling

isCancelled :: Job a -> Bool
isCancelled j = j ^. jobStatus == Cancelled

newtype EmptyHeadException = EmptyHeadException Text
  deriving (Show, Eq)

instance Exception EmptyHeadException

         -- This is a bad way of doing this. I should refactor this
         -- possibly using an Either type to indicate that there are
         -- no jobs queued and leave the exception handling to the
         -- actual exceptions.
headThrow :: (MonadThrow m, MonoFoldable mono) => mono -> m (Element mono)
headThrow l = case headMay l of
            Nothing -> throwM $ EmptyHeadException "There are no jobs queued or a job is currently active. Not doing anything."
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
remove n l = l ^.. folded . ifiltered (\i job -> i /= n || (job ^. jobStatus == Running) || (job ^. jobStatus == Cancelling))

             -- Finds the next queued job, marks it as running and returns the job.
setRunning :: TVar (JobQueue a) -> STM (Maybe (Job a))
setRunning var = do
  q <- readTVar var
  let mRes = getNotFinished q
  case mRes of
    Nothing -> return Nothing
    Just (job, q') -> do
      let job' = jobStatus .~ Running $ job
          q'' = setJob job' q'
      writeTVar var q''
      return $ Just job'

setJobStatus :: (JobStatus -> JobStatus) -> TVar (JobQueue a) -> Job a -> STM ()
setJobStatus statusUpdate var job = modifyTVar var setStatus
  where
    setStatus q = qJobs . traverse . filtered hasId . jobStatus %~ statusUpdate $ q
    hasId j = j ^. jobId == job ^. jobId

setFinished :: TVar (JobQueue a) -> Job a -> STM ()
setFinished var job = setJobStatus finishedJob var job
  where
    finishedJob Cancelling = Cancelled
    finishedJob Running = Finished
    finishedJob status = error $ "Invalid job status when finishing: " <> show status

setCancelling :: TVar (JobQueue a) -> Job a -> STM ()
setCancelling var job = setJobStatus cancellingJob var job
  where
    cancellingJob Running = Cancelling
    cancellingJob status = error $ "Invalid job status when cancelling: " <> show status
