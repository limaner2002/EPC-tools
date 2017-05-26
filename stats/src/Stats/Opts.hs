{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats.Opts where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types
import System.FilePath.Glob
import Stats
import Stats.Types (OutputMode (..), Verbosity (..))
import Control.Monad.Logger
import Control.Monad.Trans.Resource

statsInfo :: ParserInfo (IO ())
statsInfo = info (helper <*> statsParser)
  (  fullDesc
  <> progDesc "This is a tool to collect and read stats from JMeter tests."
  )

statsParser :: Parser (IO ())
statsParser = fmap (runResourceT . runStderrLoggingT) $ setLoggingLevel <$> loggingLevelParser <*>
  subparser
  (  command "raw" rawInfo
  <> command "agg" aggInfo
  )

loggingLevelParser :: Parser LogLevel
loggingLevelParser = option readLevel (short 'd') <|> pure LevelInfo

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
rawParser = dispRunsGlob <$> parseMode <*> parsePathGlob <*> parseVerbosity

aggInfo :: (MonadResource m, MonadLogger m) => ParserInfo (m ())
aggInfo = info (helper <*> aggParser)
  (  fullDesc
  <> progDesc "Gathers the aggregate reports as .csv files and puts them into a single .xlsx file."
  )

aggParser :: (MonadResource m, MonadLogger m) => Parser (m ())
aggParser = mkAggXlsx
  <$> option auto
  (  long "reports"
  <> short 'r'
  <> help "The names of the reports to display as the resulting .xlsx sheet and the path to the .csv containing the aggregate report."
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

parseVerbosity :: Parser Verbosity
parseVerbosity = flag Quiet Verbose (short 'v')

dispRunsGlob :: (MonadResource m, MonadLogger m) => OutputMode -> FilePath -> Verbosity -> m ()
dispRunsGlob mode pathGlob verbosity = do
  logDebugN $ "pathGlob: " <> tshow pathGlob

  paths <- liftBase $ namesMatching pathGlob
  dispRuns mode paths verbosity

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

setLoggingLevel :: LogLevel -> LoggingT m a -> LoggingT m a
setLoggingLevel lv = filterLogger (\_ lv' -> lv <= lv')
