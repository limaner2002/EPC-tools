{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Stats.Opts where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types
import System.FilePath.Glob
import Stats
import Stats.Types (OutputMode (..))
import Control.Monad.Logger
import Control.Monad.Trans.Resource

statsInfo :: (MonadBaseControl IO m, MonadLogger m, MonadThrow m, MonadIO m) => ParserInfo (m ())
statsInfo = info (helper <*> statsParser)
  (  fullDesc
  <> progDesc "This is a tool to collect and read stats from JMeter tests."
  )

statsParser :: (MonadBaseControl IO m, MonadLogger m, MonadThrow m, MonadIO m) => Parser (m ())
statsParser = runResourceT <$>
  subparser
  (  command "raw" rawInfo
  <> command "agg" aggInfo
  )

parsePathGlob :: Parser FilePath
parsePathGlob = strOption
  (  long "paths"
  <> short 'p'
  <> help "A list of aggregate_x_x.csv paths"
  )

rawInfo :: (MonadResource m, MonadLogger m) => ParserInfo (m ())
rawInfo = info (helper <*> rawParser)
  (  fullDesc
  <> progDesc "Gathers the data from the raw JMeter results and creates an aggregate report."
  )

rawParser :: (MonadResource m, MonadLogger m) => Parser (m ())
rawParser = dispRunsGlob <$> parseMode <*> parsePathGlob

aggInfo :: (MonadResource m, MonadLogger m) => ParserInfo (m ())
aggInfo = info (helper <*> aggParser)
  (  fullDesc
  <> progDesc "Gathers the aggregate reports as .csv files and puts them into a single .xlsx file."
  )

aggParser :: (MonadResource m, MonadLogger m) => Parser (m ())
aggParser = mkAggXlsx
  <$> ((fmap . fmap) (\(label, path) -> (label, fromString path)) $ option auto
       (  long "reports"
         <> short 'r'
         <> help "The names of the reports to display as the resulting .xlsx sheet and the path to the .csv containing the aggregate report."
       )
      )
  <*> strOption
  (  long "output"
  <> short 'o'
  <> help "The path to write the resulting .xlsx to."
  )

parseMode :: Parser OutputMode
parseMode =
  option readMode
  (  long "output-format"
  <> short 'f'
  <> help "The output format to use. Currently supported are csv and table."
  )

readMode :: ReadM OutputMode
readMode = do
  modeInput <- readerAsk
  case modeInput of
    "csv" -> pure FormatCSV
    "table" -> pure FormatTable
    "xlsx" -> pure FormatXlsx
    _ -> fail $ "Invalid argument: " <> show modeInput <> ". Valid modes are csv and table"

dispRunsGlob :: (MonadResource m, MonadLogger m) => OutputMode -> FilePath -> m ()
dispRunsGlob mode pathGlob = do
  logDebugN $ "pathGlob: " <> tshow pathGlob

  paths <- liftBase $ namesMatching pathGlob
  dispRuns mode $ fmap fromString paths

readLevel :: ReadM LogLevel
readLevel = do
  levelIn <- readerAsk
  case levelIn of
    "debug" -> return LevelDebug
    "info" -> return LevelInfo
    "warn" -> return LevelWarn
    "error" -> return LevelError
    "other" -> return $ LevelOther "other"
    _ -> fail "Invalid log level."
