{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

import ClassyPrelude
import Test.QuickCheck
import Scheduler.Types
import Control.Lens hiding (elements, cons)
import Scheduler
import Control.Monad.State

main :: IO ()
main = putStrLn "Test suite not yet implemented"

instance Arbitrary a => Arbitrary (Job a) where
  arbitrary = do
    n <- arbitrary
    s <- arbitrary
    v <- arbitrary
    newJob' n s v
    where
      newJob' name status a = evalStateT (newJob name status a) 0

instance Arbitrary JobStatus where
  arbitrary = elements
    [ Queued
    , Running
    , Finished
    , Cancelling
    , Cancelled
    ]

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

-- instance Arbitrary a => Arbitrary (JobQueue a) where
--   arbitrary = (\a -> qJobs .~ a $ emptyQueue) <$> jobList
--     where
--       jobList = mapM (\s -> Job <$> arbitrary <*> pure s <*> arbitrary) =<< statusList

-- newtype ActiveStatus = ActiveStatus {getActiveStatus :: JobStatus}
--   deriving (Show, Eq)

-- instance Arbitrary ActiveStatus where
--   arbitrary = ActiveStatus <$> elements activeStatuses

-- statusList :: Gen [JobStatus]
-- statusList = do
--   pre <- listOf $ elements [Finished, Cancelled]
--   mMid <- arbitrary
--   case mMid of
--     Nothing -> return pre
--     Just mid -> do
--       post <- listOf (pure Queued)
--       return $ pre <> [getActiveStatus mid] <> post

-- isActiveStatus :: JobStatus -> Bool
-- isActiveStatus job = job `elem` activeStatuses

-- activeStatuses :: [JobStatus]
-- activeStatuses = [Running, Cancelling]

-- anyActive :: JobQueue a -> Bool
-- anyActive q = any isActiveStatus $ q ^.. qJobs . traverse . jobStatus

-- anyQueued :: JobQueue a -> Bool
-- anyQueued q = any isQueued $ q ^. qJobs

-- anyCancelling :: JobQueue a -> Bool
-- anyCancelling q = any isCancelling $ q ^. qJobs

-- notFinishedProp :: JobQueue a -> Bool
-- notFinishedProp q = case anyActive q of
--   True -> case getNotFinished q of
--     Nothing -> True
--     Just _ -> False             -- This should never happen if any job is active.
--   False -> case getNotFinished q of
--     Nothing -> not $ anyQueued q
--     Just (job, q') -> isActiveStatus (job ^. jobStatus) && anyActive q'

-- runningProp :: JobQueue a -> Bool
-- runningProp q = case any isRunning (q ^. qJobs) of
--   True -> case cancelRunning q of
--     Nothing -> False
--     Just (job, q') -> isRunning job && anyCancelling q'
--   False -> case cancelRunning q of
--     Nothing -> True
--     Just _ -> False             -- This should never happen if no job is active
