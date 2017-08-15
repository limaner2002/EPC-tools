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
import Scheduler.Types (emptyQueue, JobQueue)
import Servant as S
import Control.Monad.Logger
import Scheduler.Google (serverSettings, epcEnv, serverSettings)
import Scheduler.Google.Server hiding (runServer)
import qualified Scheduler.Google.Server as SGS
import Data.Aeson
import System.Directory

runServer :: (MonadLogger m, MonadIO m, ToJSON a) => TMVar Int -> (FilePath -> FilePath -> Text -> ExceptT ServantErr IO (Text, a)) -> Kleisli (ExceptT ServantErr IO) a () -> Port -> FilePath -> FilePath -> FilePath -> FilePath -> m ()
runServer tmVar mkJob jobAct port downloadRootDir staticDir jmeterPath resultDir = do
  liftIO $ do
    createDirectoryIfMissing True resultDir
    setCurrentDirectory resultDir
  v <- liftIO $ newTVarIO emptyQueue
  env <- liftIO $ epcEnv
  jobIdT <- liftIO $ newTVarIO 0
  let settings = serverSettings downloadRootDir staticDir env
      schedSettings = SchedulerSettings (\jmxPath cfgPath -> S.Handler $ mkJob jmeterPath jmxPath cfgPath) (toHandler jobAct) tmVar resultDir v jobIdT
  liftIO $ run port $ serve combinedProxy $ combinedServer schedSettings settings

schedulerInfo :: (MonadLogger m, MonadIO m, ToJSON a) => TMVar Int -> (FilePath -> FilePath -> Text -> ExceptT ServantErr IO (Text, a)) -> Kleisli (ExceptT ServantErr IO) a () -> ParserInfo (m ())
schedulerInfo tmVar mkJob jobAct = info (helper <*> schedulerParser tmVar mkJob jobAct)
  (  fullDesc
  <> header "Test scheduler UI."
  <> progDesc "Runs a simple server which allows for scheduling of tests."
  )

schedulerParser :: (MonadLogger m, MonadIO m, ToJSON a) => TMVar Int -> (FilePath -> FilePath -> Text -> ExceptT ServantErr IO (Text, a)) -> Kleisli (ExceptT ServantErr IO) a () -> Parser (m ())
schedulerParser tmVar mkJob jobAct = runServer tmVar <$>
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
  <*> strOption
  (  long "jmeter-path"
  <> short 'j'
  <> metavar "JMETER_PATH"
  <> help "The directory containing the jMeter executables."
  )
  <*> strOption
  (  long "result-dir"
  <> short 'o'
  <> metavar "RESULT_DIR"
  <> help "The directory to place the results in."
  )

toHandler :: Kleisli (ExceptT ServantErr IO) a b -> Kleisli S.Handler a b
toHandler act = Kleisli (S.Handler . runKleisli act)
