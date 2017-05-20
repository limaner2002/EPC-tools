{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats.Opts
  ( statsInfo
  ) where

import ClassyPrelude
import Options.Applicative
import System.FilePath.Glob
import Stats

statsInfo :: ParserInfo (IO ())
statsInfo = info (helper <*> statsParser)
  (  fullDesc
  <> progDesc "This is a tool to collect and read stats from JMeter tests."
  )

parsePaths :: Parser (IO [FilePath])
parsePaths =
  namesMatching <$>
  strOption
  (  long "paths"
  <> short 'p'
  <> help "A list of aggregate_x_x.csv paths"
  )

statsParser :: Parser (IO ())
statsParser = (\paths -> join $ dispRuns <$> paths) <$> parsePaths
       

