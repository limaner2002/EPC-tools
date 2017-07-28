{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import ClassyPrelude
import Execute

import ConfigFile
import Types ( JMeterOpts (..)
             , ToScheduledTime (..)
             , mkScheduledTime
             , ScheduledTime
             , createBatchOpts
             , BatchOpts
             , Validated
             , LogSettings (..)
             , Run (..)
             , NUsers (..)
             )
import Data.Time
import qualified System.IO as SIO

import Options.Applicative
import Options.Applicative.Types
import Control.Monad.Catch
import Control.Monad.Trans.State

import Path
import Network.Wai.Handler.Warp
import SendMail
import Results
import Control.Arrow (Kleisli (..))
import Logs.Opts

import Validate ( validateBatchOpts,
                  ValidationResult (..)
                )

import Table
import Scheduler.Opts
import Plot.Opts
import Stats.Opts
import Control.Monad.Logger
import Development.GitRev

parseMany :: ReadM [String]
parseMany = readerAsk >>= pure . words

parseToScheduledTime :: Parser ToScheduledTime
parseToScheduledTime =
  At <$> option (readTime' "%X %Z")
  (  long "at"
  <> help "Schedule the tests for a specific time and day."
  )
  <|>
  TOD <$> option (readTime' "%X %Z")
  (  long "tod"
  <> help "Schedule the tests for a specific time of day. If the time of day is before the current time, the tests will be scheduled for the next day at that time."
  )

readTime' :: ParseTime a => String -> ReadM a
readTime' fmt = do
  input <- readerAsk
  parseTimeM True defaultTimeLocale fmt input

parseCommands :: (MonadLogger m, MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadMask m) => TimeZone -> Parser (m ())
parseCommands tz = subparser
  (  command "analyse" resultsInfo
  <> command "download-log" logsInfo
  <> command "make-table" tableInfo
  <> command "scheduler-ui" (schedulerInfo (toJMeterOpts . unpack) (Kleisli (lift . runJMeter)))
  <> command "new-scheduler-ui" newSchedulerInfo
  <> command "plot-metrics" (plotInfo tz)
  <> command "get-stats" statsInfo
  )

commandsInfo :: (MonadLogger m, MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadMask m) => TimeZone -> ParserInfo (m ())
commandsInfo tz = info (helper <*> parseCommands tz)
  (  fullDesc
  <> progDesc progInfo
  )

progInfo = "This is a collection of various tools to help aid in EPC Post Commit performance testing.\n"
  <> $(gitHash) <> "\n("
  <> $(gitCommitDate) <> ")\n"

loggingLevelParser :: Parser LogLevel
loggingLevelParser = option readLevel (short 'd') <|> pure LevelInfo

main :: IO ()
main = do
  tz <- getCurrentTimeZone
  SIO.hSetBuffering stdout SIO.NoBuffering
  runStderrLoggingT . (setLoggingLevel LevelInfo) =<< execParser (commandsInfo tz)

setLoggingLevel :: LogLevel -> LoggingT m a -> LoggingT m a
setLoggingLevel lv = filterLogger (\_ lv' -> lv <= lv')

data ExecutionStatus
  = NotStarted
  | Running
  | Finished

readJMeterOpts :: MonadThrow m => ExecuteType -> Config -> m JMeterOpts
readJMeterOpts execType cfg =
  JMeterOpts <$> dryVal execType (pure $ Run 1) (getVal' "nRuns")
             <*> dryVal execType (pure $ [NUsers 1]) (getVal' "nUsers")
             <*> getVal' "jmxPath"
             <*> getVal' "jmeterPath"
             <*> getVal' "runName"
             <*> (replaceIt <$> getVal' "otherOpts")
             <*> getVal' "sleepTime"
  where
    getVal' k = getVal k cfg
                -- This is a quick and dirty hack. I need to update
                -- hs-config to properly handle strings that contain
                -- '=' characters.
    replaceIt :: [Text] -> [Text]
    replaceIt = fmap (replaceElem '@' '=')
    dryVal DryRun f _ = f
    dryVal Execute _ f = f

data ExecuteType
  = Execute
  | DryRun

-- runTests :: ExecuteType -> ToScheduledTime -> [FilePath] -> IO ()
-- runTests execType timestamp configFiles = do
--   tz <- getCurrentTimeZone
--   cfgs <- mapM (readConfigFile . unpack) configFiles
--   let eOpts = sequence $ fmap (readJMeterOpts execType) cfgs
--   case eOpts of
--     Left msg -> fail $ show msg
--     Right opts -> do
--       ct <- getCurrentTime
--       let time' = mkScheduledTime tz timestamp ct
--           sendScheduledTime = mkScheduledTime tz sendScheduledInfo ct
--           checkJobStatusTime' = mkScheduledTime tz checkJobStatusTime ct
--       initialize tz time' sendScheduledTime checkJobStatusTime' opts

-- initialize :: TimeZone -> ScheduledTime -> ScheduledTime -> ScheduledTime -> [JMeterOpts] -> IO ()
-- initialize tz jobTime sendScheduledTime checkJobStatusTime' opts = do
--   (validatedOpts, vr) <- runStateT (validateBatchOpts $ createBatchOpts opts) mempty
--   let dispMsg (rn, msg) = show rn <> "\n" <> msg <> "\n\n"
--   print vr
--   mapM_ (putStr . pack . dispMsg) $ valMsgs vr
--   SIO.hFlush stdout
--   runningStatus <- newTVarIO NotStarted
--   _ <- concurrently
--     ( do
--         schedule (trace ("scheduledMsg: " <> show sendScheduledTime) sendScheduledTime) $ do
--           putStrLn "Sending message now"
--           sendMessage $ scheduledMessage validatedOpts sendScheduledTime
--         schedule (trace ("checkMsg: " <> show checkJobStatusTime') checkJobStatusTime') $ checkStatus tz runningStatus validatedOpts
--     )
--     (
--       doIfDirIsEmpty $ do
--         atomically $ writeTVar runningStatus Running
--         sendMessage $ scheduledMessage validatedOpts jobTime
--         schedule jobTime $ batchJMeterScripts $ validatedOpts
--         atomically $ writeTVar runningStatus Finished
--         clt <- utcToLocalTime tz <$> getCurrentTime
--         sendMessage $ jobsCompletedMessage clt
--     )
--   return ()

-- checkStatus :: TimeZone -> TVar ExecutionStatus -> BatchOpts Validated -> IO ()
-- checkStatus tz runningStatus opts = go
--   where
--     go = do
--       putStrLn "Sending message now"
--       SIO.hFlush stdout
--       isRunning <- atomically $ readTVar runningStatus
--       clt <- utcToLocalTime tz <$> getCurrentTime
--       case isRunning of
--         Running -> do
--           sendMessage $ stillRunningMessage opts clt
--           threadDelay 600000000
--           go
--         Finished -> sendMessage $ nothingScheduledMessage clt
--         NotStarted -> do
--           sendMessage $ notStartedMessage clt
--           threadDelay 600000000
--           go

-- toTimeOfDay :: Text -> TimeOfDay
-- toTimeOfDay txt = fromJust $ readMay txt
--   where
--     -- The value is hardcoded for now so this is safe to use
--     fromJust (Just t) = t
--     fromJust _ = error "Should not have been called like this! Moreso this function should be removed."

-- checkJobStatusTime :: ToScheduledTime
-- checkJobStatusTime = TOD $ toTimeOfDay "21:00:00"

-- sendScheduledInfo :: ToScheduledTime
-- sendScheduledInfo = TOD $ toTimeOfDay "08:00:00"

toJMeterOpts :: (MonadThrow m, MonadIO m) => FilePath -> m JMeterOpts
toJMeterOpts fp = do
  cfg <- readConfigFile fp
  readJMeterOpts Execute cfg
