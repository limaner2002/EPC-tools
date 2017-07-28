{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Opts
  ( schedulerInfo
  , newSchedulerInfo
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
import Scheduler.Google (serverSettings, epcEnv)
import qualified Scheduler.Google.Server as SGS

runServer :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> Port -> m ()
runServer mkJob jobAct port = do
  v <- liftIO $ newTVarIO emptyQueue
  liftIO $ run port $ application (S.Handler . mkJob) (toHandler jobAct) v

runNewServer :: (MonadLogger m, MonadIO m) => Port -> FilePath -> FilePath -> m ()
runNewServer port downloadRootDir staticDir = do
  env <- liftIO epcEnv
  liftIO $ run port $ SGS.runServer $ serverSettings downloadRootDir staticDir env

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

newSchedulerInfo :: (MonadLogger m, MonadIO m) => ParserInfo (m ())
newSchedulerInfo = info (helper <*> newSchedulerParser)
  (  fullDesc
  <> header "New EPC Scheduler UI."
  <> progDesc "This runs the new scheduler UI which utilizes Google's Drive API to keep files in sync."
  )

newSchedulerParser :: (MonadLogger m, MonadIO m) => Parser (m ())
newSchedulerParser = runNewServer
  <$> option auto
  (  long "port"
  <> short 'p'
  <> metavar "PORT"
  <> help "The port to run the server on."
  )
  <*> strOption
  (  long "root"
  <> short 'r'
  <> metavar "DOWNLOAD_ROOT"
  <> help "The root directory in which to place the .zip files downloaded from Drive."
  )
  <*> strOption
  (  long "static-dir"
  <> short 's'
  <> metavar "STATIC_DIR"
  <> help "The directory to serve the .js, .html, etc. files for the GHCjs UI."
  )

toHandler :: Kleisli (ExceptT ServantErr IO) a b -> Kleisli S.Handler a b
toHandler act = Kleisli (S.Handler . runKleisli act)
