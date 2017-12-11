{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.ProducerConsumer
  ( runParallel
  , Parallel (..)
  , nThreads
  ) where

import ClassyPrelude
import qualified Streaming.Prelude as S
import Control.Arrow

data DistributeTask
  = Produce
  | Consume
  deriving Show

distributeTasks :: MonadIO m => TVar DistributeTask -> TChan (ThreadControl a) -> S.Stream (S.Of (ThreadControl a)) m c -> (a -> m b) -> m (Maybe b)
distributeTasks taskVar chan producer f = do
  task <- g
  case task of
    Produce -> do
      S.mapM_ (atomically . writeTChan chan) $ producer
      atomically $ writeTChan chan Finished
      consumer
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
      sequence $ fmap f mA

data ThreadControl a
  = Item a
  | Finished
  deriving Show

newtype NThreads = NThreads Int
    deriving Show

nThreads :: Int -> NThreads
nThreads = NThreads

data Parallel (m :: * -> *) a b c = Parallel
    { parNThreads :: NThreads
    , parProducer :: S.Stream (S.Of a) m b
    , parConsumer :: a -> m c
    }

runParallel :: (MonadMask m, MonadBaseControl IO m, Forall (Pure m), MonadIO m, Show a)
    => Parallel m a b c -> m [Maybe c]
runParallel par = bracket makeChans (uncurry resetConf) runParallel'
    where
        runParallel' (var, chan) = mapConcurrently (const $ distributeTasks var chan (S.map Item $ producer) consumer) [1..n]
        consumer = parConsumer par
        producer = parProducer par
        (NThreads n) = parNThreads par

makeChans :: MonadIO m => m (TVar DistributeTask, TChan (ThreadControl a))
makeChans = do
    var <- atomically $ newTVar Produce
    chan <- atomically $ newTChan
    return (var, chan)
    
resetConf :: (Show a, MonadIO m) => TVar DistributeTask -> TChan (ThreadControl a) -> m ()
resetConf var chan = do
    putStrLn "Resetting config!"
    S.reread (atomically . tryReadTChan) >>> S.print $ chan
    atomically $ writeTVar var Produce

