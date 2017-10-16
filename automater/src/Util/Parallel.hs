{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Parallel
  ( newParallelConf
  , runParallel
  , distributeTasks
  , runDistributeTasks
  , ParallelConf
  ) where

import ClassyPrelude
import qualified Streaming.Prelude as S
import Control.Arrow
import Control.Monad.Catch ()
import Control.Monad.Logger

data DistributeTask
  = Produce
  | Consume
  deriving Show

data Parallel (m :: * -> *) a = Parallel
    { parNThreads :: NThreads
    , parProducer :: S.Stream (S.Of a) m ()
    , parConsumer :: a -> m ()
    }

data ParallelConf a = ParallelConf
  { confTaskVar :: TVar DistributeTask
  , confTaskChan :: TChan (ThreadControl a)
  , confActiveThreads :: TVar (Positive Int)
  }

newtype ParallelT b (m :: * -> *) a = ParallelT { unParallel :: ReaderT (ParallelConf b) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadMask, MonadCatch, MonadThrow, MonadReader (ParallelConf b), MonadTrans, MonadLogger)

newtype DistributeTasksT b (m :: * -> *) a = DistributeTasksT
  { runDistributeTasksT :: ParallelT b m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadMask, MonadCatch, MonadThrow, MonadReader (ParallelConf b), MonadTrans, MonadLogger)

runParallel :: (Forall (Pure m), MonadBaseControl IO m) => ParallelT b m a -> ParallelConf b -> NThreads -> m [a]
runParallel par conf (NThreads n) = do
  let active = confActiveThreads conf
  activeThreads <- liftBase $ mkPositive n
  liftBase $ atomically $ writeTVar active activeThreads
  mapConcurrently (flip runParallel' conf) $ take n $ repeat par

runParallel' :: ParallelT b m a -> ParallelConf b -> m a
runParallel' = runReaderT . unParallel

runDistributeTasks :: (MonadMask m, MonadIO m, MonadLogger m) => DistributeTasksT b m a -> ParallelT b m a
runDistributeTasks d = runDistributeTasksT $ finally d resetConf

distributeTasks :: (MonadIO m, MonadMask m) => S.Stream (S.Of (ThreadControl a)) m () -> (a -> m b) -> DistributeTasksT a m (Maybe b)
distributeTasks producerStrm f = DistributeTasksT $ ParallelT $ ReaderT $ distributeTasks_ producerStrm f

distributeTasks_ :: (MonadIO m, MonadCatch m) => S.Stream (S.Of (ThreadControl a)) m () -> (a -> m b) -> ParallelConf a -> m (Maybe b)
distributeTasks_ producerStrm f conf = do
  task <- g
  case task of
    Produce -> catchAny producer except
    Consume -> consumer
  where
    g = atomically $ do
      task <- readTVar taskVar
      case task of
        Produce -> do
          writeTVar taskVar Consume
          return task
        _ -> return Consume
    consumer = do
      mA <- atomically $ do
          tc <- readTChan chan
          case tc of
              Finished -> do
                  writeTChan chan Finished
                  return Nothing
              Item a -> return (Just a)
              DeadProducer -> do
                writeTChan chan DeadProducer
                throwM DeadProducerException
      sequence $ fmap f mA
    taskVar = confTaskVar conf
    chan = confTaskChan conf
    producer = do
      S.mapM_ (atomically . writeTChan chan) $ producerStrm
      atomically $ writeTChan chan Finished
      consumer
    except e = do
      atomically $ writeTChan chan DeadProducer
      throwM e

data ThreadControl a
  = Item a
  | Finished
  | DeadProducer
  deriving Show

data DeadProducerException = DeadProducerException
  deriving Show

instance Exception DeadProducerException

newtype ZeroException = ZeroException Text
    deriving Show
    
instance Exception ZeroException

producer :: MonadThrow m => [Int] -> S.Stream (S.Of Int) m ()
producer = S.each >>> S.mapM mThrow
    where
        mThrow 0 = error "A number is 0!"
        mThrow n = return n
        
resetConf :: (MonadIO m, MonadLogger m) => DistributeTasksT a m ()
resetConf = do
  n <- DistributeTasksT $ ParallelT $ ReaderT resetConf_
  logDebugN $ "n: " <> tshow n

resetConf_ :: MonadIO m => ParallelConf a -> m (Positive Int)
resetConf_ conf = atomically $ do
  let active = confActiveThreads conf
  modifyTVar active decPositive
  n <- readTVar active
  case n of
    (Positive 0) -> reset >> return n
    _ -> return n
  where
    reset = do
      let var = confTaskVar conf
          chan = confTaskChan conf
      S.reread tryReadTChan >>> S.mapM_ (const $ pure ()) $ chan
      writeTVar var Produce

newtype NonPositiveException a = NonPositiveException a

instance Show a => Show (NonPositiveException a) where
  show (NonPositiveException a) = "NonPositiveException: \"Cannot create a Positive using " <> show a <> "\""

instance (Typeable a, Show a) => Exception (NonPositiveException a)

newtype Positive a = Positive { unPositive :: a }

instance Show a => Show (Positive a) where
  show pos = "Positive { unPositive = " <> show (unPositive pos) <> " }"

mkPositive :: (MonadThrow m, Ord a, Num a, Typeable a, Show a) => a -> m (Positive a)
mkPositive n
  | n >= 0 = return $ Positive n
  | otherwise = throwM $ NonPositiveException n

decPositive :: (Eq a, Num a, Integral a) => Positive a -> Positive a
decPositive (Positive 0) = Positive 0
decPositive (Positive n) = Positive (n - 1)

decPositive' :: (Num a, MonadThrow m, Typeable a, Show a, Ord a, Integral a) => Positive a -> m (Positive a)
decPositive' (Positive n) = mkPositive (n - 1)

incPositive :: (Num a, Integral a) => Positive a -> Positive a
incPositive (Positive n) = Positive (n + 1)

newtype NThreads = NThreads Int
    deriving Show

nThreads :: Int -> NThreads
nThreads = NThreads

newParallelConf :: IO (ParallelConf Int)
newParallelConf = ParallelConf
  <$> newTVarIO Produce
  <*> newTChanIO
  <*> (newTVarIO =<< (mkPositive 0))

