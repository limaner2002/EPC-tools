{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Opts
  ( schedulerInfo
  ) where

import ClassyPrelude
import Options.Applicative
import Network.Wai.Handler.Warp
import Control.Arrow
import Control.Monad.Trans.Except
import Scheduler.Server
import Scheduler.Types (emptyQueue)
import Servant as S
import Control.Monad.Logger

runServer :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> Port -> m ()
runServer mkJob jobAct port = do
  v <- liftIO $ newTVarIO emptyQueue
  liftIO $ run port $ application (S.Handler . mkJob) (toHandler jobAct) v

schedulerInfo :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> ParserInfo (m ())
schedulerInfo mkJob jobAct = info (helper <*> schedulerParser mkJob jobAct)
  (  fullDesc
  <> header "Test scheduler UI."
  <> progDesc "Runs a simple server which allows for scheduling of tests."
  )

schedulerParser :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> Parser (m ())
schedulerParser mkJob jobAct = runServer <$>
  pure mkJob
  <*>
  pure jobAct
  <*>
  option auto
  (  long "port"
  <> short 'p'
  <> metavar "PORT"
  <> help "The port to run the server on."
  )

toHandler :: Kleisli (ExceptT ServantErr IO) a b -> Kleisli S.Handler a b
toHandler act = Kleisli (S.Handler . runKleisli act)
