{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler.Arrow
  ( runJobs
  , getQueue
  , setQueue
  ) where

import qualified Scheduler as S
import Control.Arrow
import ClassyPrelude
import Scheduler.Types
import Control.Lens
import MachineUtils

getNotFinished :: MonadThrow m => Kleisli m (JobQueue a) (Job a, JobQueue a)
getNotFinished = Kleisli S.getNotFinished

checkRunning :: MonadThrow m => Kleisli m (Job a) (Job a)
checkRunning = Kleisli S.checkRunning

checkQueueRunning :: Kleisli STM (TVar (JobQueue a)) (JobQueue a)
checkQueueRunning = Kleisli (\v -> readTVar v >>= S.checkQueueRunning)

-- checkQueueRunning :: Kleisli STM (TVar (JobQueue a)) (Either QueueException (JobQueue a))
-- checkQueueRunning = Kleisli (\v -> catchQueueException (readTVar v >>= S.checkQueueRunning))

catchQueueException :: MonadCatch m => m a -> m (Either QueueException a)
catchQueueException f = catch (Right <$> f) catchQueueException_
  where
    catchQueueException_ exc = pure $ Left exc

               -- Will throw an exception if a job is already running.
getNextQueued :: MonadThrow m => Kleisli m (JobQueue a) (Job a, JobQueue a)
getNextQueued = Kleisli S.checkQueueRunning >>> getNotFinished -- getNotFinished >>> checkRunning *** id

addJob :: Arrow a => a (Job b, JobQueue b) (JobQueue b)
addJob = arr (uncurry S.addJob)

getQueue :: MonadBase IO m => Kleisli m (TVar (JobQueue a)) (JobQueue a)
getQueue = Kleisli S.getQueue

setQueue :: MonadBase IO m => Kleisli m (TVar (JobQueue a), JobQueue a) ()
setQueue = Kleisli (uncurry S.setQueue)

setJob :: Arrow a => a (Job b, JobQueue b) (JobQueue b)
setJob = arr (uncurry S.setJob)

runJob :: MonadBase IO m => Kleisli m a () -> Kleisli m (TVar (JobQueue a)) ()
runJob f = proc var -> do
  q <- getQueue -< var
  res <- arr (runKleisli getNextQueued) -< q
  case res of
    Left exc -> Kleisli (liftBase . print) -< exc
    Right (job, q) -> do
      _ <- setQueue -< (var, q)
      _ <- passthroughK (\j -> liftBase $ putStrLn $ "Running job " <> tshow (j ^. jobName)) >>> arr _jobVal >>> f -< job
      q' <- arr (jobStatus .~ Finished) *** id >>> setJob -< (job, q)
      _ <- setQueue -< (var, q')
      runJob f -< var

setQueueRunning :: Arrow cat => cat (JobQueue a) (JobQueue a)
setQueueRunning = arr (qStatus .~ QRunning)

runJobs :: MonadBase IO m => Kleisli m a () -> TVar (JobQueue a) -> m ()
runJobs f var = runKleisli (runJob f) var

-- catchTest :: (MonadIO m, MonadCatch m) => TVar (JobQueue a) -> m (Either QueueException (JobQueue a))
-- catchTest v = catch f g
--   where
--     f = Right <$> (atomically $ runKleisli checkQueueRunning v)
--     g exc@(QueueException _) = pure $ Left exc