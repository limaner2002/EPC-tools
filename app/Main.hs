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
             , runName
             , getRunName
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
import Control.Lens ((^.))

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
  <> command "scheduler-ui" (schedulerInfo (fmap (\jo -> (jo ^. runName . getRunName, jo)) . toJMeterOpts . unpack) (Kleisli (lift . runJMeter)))
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

toJMeterOpts :: (MonadThrow m, MonadIO m) => FilePath -> m JMeterOpts
toJMeterOpts fp = do
  cfg <- readConfigFile fp
  readJMeterOpts Execute cfg
