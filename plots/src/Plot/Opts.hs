{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plot.Opts
  ( plotInfo
  ) where

import ClassyPrelude hiding (parseTime)
import Options.Applicative
import Options.Applicative.Types
import Plot.Metrics
import Data.Time hiding (readTime, parseTime)
import Data.Time.Clock.POSIX

plotInfo :: TimeZone -> ParserInfo (IO ())
plotInfo tz = info (helper <*> plotParser tz)
  (  fullDesc
  <> header "System Metrics Plotter"
  <> progDesc "Currently only supports line plots with a single metric. Supports one or more nodes."
  )

plotParser :: TimeZone -> Parser (IO ())
plotParser tz = plotSystem'
  <$> pt "start"
  <*> pt "end"
  <*> strOption
  (  long "prefix"
  <> metavar "PREFIX"
  <> help "The file path prefix to use for the metric files."
  )<*> option auto
  (  long "column"
  <> short 'c'
  <> metavar "COLUMN"
  <> help "The number of the column to use from left to right starting at 0."
  )<*> strOption
  (  long "yAxisTitle"
  <> short 'y'
  <> metavar "Y_AXIS_TITLE"
  <> help "The title to use on the y-axis."
  )
  where
    pt cmd = subparser $ command cmd (parseTimeInfo tz)

readTime :: ReadM LocalTime
readTime = do
  input <- readerAsk
  case readMay input of
    Nothing -> readerError $ show input <> " does not appear to be a valid time. Please use the format 'YYYY-MM-DD HH:MM:SS TZ'"
    Just time -> return time

readJMeter :: TimeZone -> ReadM LocalTime
readJMeter tz = do
  input <- readerAsk
  case readMay input of
    Nothing -> readerError $ show input <> " does not appear to be a valid JMeter timestamp."
    Just t -> return $ utcToLocalTime tz $ posixSecondsToUTCTime (fromInteger t / 1000)

parseTimeInfo :: TimeZone -> ParserInfo LocalTime
parseTimeInfo tz = info (helper <*> parseTime tz) fullDesc

parseTime :: TimeZone -> Parser LocalTime
parseTime tz =
  option readTime
   (  short 't'
   <> help "Give the time in 'YYYY-MM-DD HH:MM:SS TZ' format."
   )
  <|> option (readJMeter tz)
   (  short 'p'
   <> help "Give the time in POSIX milliseconds. This is the format that JMeter uses in the raw data files."
   )
