{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats.Opts
  ( statsInfo
  ) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types
import System.FilePath.Glob
import Stats
import Stats.Types (OutputMode (..), Verbosity (..))

statsInfo :: ParserInfo (IO ())
statsInfo = info (helper <*> statsParser)
  (  fullDesc
  <> progDesc "This is a tool to collect and read stats from JMeter tests."
  )

parsePathGlob :: Parser FilePath
parsePathGlob = strOption
  (  long "paths"
  <> short 'p'
  <> help "A list of aggregate_x_x.csv paths"
  )

statsParser :: Parser (IO ())
statsParser = dispRunsGlob <$> parseMode <*> parsePathGlob <*> parseVerbosity

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
    "csv" -> pure CSV
    "table" -> pure Table
    _ -> fail $ "Invalid argument: " <> show modeInput <> ". Valid modes are csv and table"

parseVerbosity :: Parser Verbosity
parseVerbosity = flag Quiet Verbose (short 'v')

dispRunsGlob :: OutputMode -> FilePath -> Verbosity -> IO ()
dispRunsGlob mode pathGlob verbosity = do
  case verbosity of
    Verbose ->
      putStrLn $ "pathGlob: " <> tshow pathGlob
    Quiet -> return ()

  paths <- namesMatching pathGlob
  dispRuns mode paths verbosity
