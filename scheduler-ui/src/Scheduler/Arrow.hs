{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler.Arrow
  ( runJobs
  , getQueue
  , setQueue
  , moveJobUpK
  , moveJobDownK
  , removeJobK
  ) where

import qualified Scheduler as S
import Control.Arrow
import ClassyPrelude
import Scheduler.Types
import Control.Lens
import Scheduler.Util
import System.Directory

getNotFinished :: MonadThrow m => Kleisli m (JobQueue a) (Job a, JobQueue a)
getNotFinished = Kleisli S.getNotFinished

checkRunning :: MonadThrow m => Kleisli m (Job a) (Job a)
checkRunning = Kleisli S.checkRunning

checkQueueRunning :: Kleisli STM (TVar (JobQueue a)) (JobQueue a)
checkQueueRunning = Kleisli (readTVar >=> S.checkQueueRunning)

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
  job <- Kleisli (liftBase . atomically . S.setRunning) -< var
  _ <- passthroughK (\j -> liftBase $ putStrLn $ "Running job " <> tshow (j ^. jobName)) >>> arr _jobVal >>> f -< job
  Kleisli (liftBase . atomically . uncurry S.setFinished) -< (var, job)

runJobs :: (MonadBase IO m, MonadCatch m, MonadThrow m) => Kleisli m a () -> TVar (JobQueue a) -> m ()
runJobs f var = do
  dir <- liftBase $ createTodayDirectory "."
  cwd <- liftBase getCurrentDirectory
  liftBase $ setCurrentDirectory dir
  res <- tryAny $ runKleisli (runJob f) var
  case res of
    Left exc -> do
      liftBase $ setCurrentDirectory cwd
      throw exc
    Right r -> do
      liftBase $ setCurrentDirectory cwd
      return r

passthroughK :: Monad m => (a -> m ()) -> Kleisli m a a
passthroughK f = proc a -> do
  _ <- Kleisli f -< a
  returnA -< a

moveJobUpK :: MonadBase IO m => Int -> Kleisli m (TVar (JobQueue a)) ()
moveJobUpK idx = id &&& getQueue
  >>> id *** arr (qJobs %~ S.moveUp idx)
  >>> setQueue

moveJobDownK :: MonadBase IO m => Int -> Kleisli m (TVar (JobQueue a)) ()
moveJobDownK idx = id &&& getQueue
  >>> id *** arr (qJobs %~ S.moveBack idx)
  >>> setQueue

removeJobK :: MonadBase IO m => Int -> Kleisli m (TVar (JobQueue a)) ()
removeJobK idx = id &&& getQueue
  >>> id *** arr (qJobs %~ S.remove idx)
  >>> setQueue
