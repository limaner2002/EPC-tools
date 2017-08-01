{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Scheduler.Opts
  ( schedulerInfo
  , runServer
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
import Scheduler.Google (serverSettings, epcEnv, serverSettings)
import Scheduler.Google.Server hiding (runServer)
import qualified Scheduler.Google.Server as SGS

runServer :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO (Text, a)) -> Kleisli (ExceptT ServantErr IO) a () -> Port -> FilePath -> FilePath -> m ()
runServer mkJob jobAct port downloadRootDir staticDir = do
  v <- liftIO $ newTVarIO emptyQueue
  env <- liftIO $ epcEnv
  let settings = serverSettings downloadRootDir staticDir env
  liftIO $ run port $ serve combinedProxy $ combinedServer (S.Handler . mkJob) (toHandler jobAct) settings v

schedulerInfo :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO (Text, a)) -> Kleisli (ExceptT ServantErr IO) a () -> ParserInfo (m ())
schedulerInfo mkJob jobAct = info (helper <*> schedulerParser mkJob jobAct)
  (  fullDesc
  <> header "Test scheduler UI."
  <> progDesc "Runs a simple server which allows for scheduling of tests."
  )

schedulerParser :: (MonadLogger m, MonadIO m) => (Text -> ExceptT ServantErr IO (Text, a)) -> Kleisli (ExceptT ServantErr IO) a () -> Parser (m ())
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
