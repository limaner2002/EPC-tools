{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Scripts.Parseable
    (module Scripts.Parseable
    ) where

import ClassyPrelude
import Options.Applicative
import Appian.Client
import Appian.Instances
import Appian
import Util.Parallel (LogFilePath, logFilePath, runParallelFileLoggingT)
import Stats.CsvStream (CsvPath)
import Scripts.Execute

class HasOption a where
  parseOption :: Parser a

newtype NUsers = NUsers Int
  deriving Show
  
newtype LogPath = LogPath FilePath
  deriving Show

instance HasOption NUsers where
  parseOption = NUsers
    <$> option auto
    (  long "--num-users"
    <> short 't'
    )
    
instance HasOption LogPath where
  parseOption = LogPath
    <$> strOption
    (  long "--log-path"
    <> short 'l'
    )

instance HasOption Bounds where
  parseOption = boundsParser

boundsParser :: Parser Bounds
boundsParser = Bounds
  <$> option auto
  (  long "lower"
  <> help "The minimum for the think timer"
  )
  <*> option auto
  (  long "upper"
  <> help "The maximum for the think timer"
  )

instance HasOption HostUrl where
  parseOption = hostUrlParser

hostUrlParser :: Parser HostUrl
hostUrlParser = HostUrl
  <$> strOption
  (  long "host-name"
  <> short 'n'
  <> help "The hostname of the server to use."
  )

instance HasOption LogMode where
  parseOption = logModeParser

logModeParser :: Parser LogMode
logModeParser = (
  strOption
  (  long "stdout"
  <> help "Log messages to stdout."
  )) *> pure LogStdout
  <|>
  LogFile <$> logFileParser

logFileParser :: Parser LogFilePath
logFileParser = logFilePath <$> strOption
  (  long "log-file-path"
  <> short 'l'
  <> help "The path of the file to write the logs to."
  )

instance HasOption CsvPath where
  parseOption = csvConfParser

csvConfParser :: Parser CsvPath
csvConfParser = fromString <$>
  strOption
  (  long "csv-conf"
  <> short 'i'
  <> help "The csv config file for 471 intake."
  )

instance HasOption RampupTime where
  parseOption = rampupParser

rampupParser :: Parser RampupTime
rampupParser = mkRampup
  <$> option auto
     (  long "rampup"
     <> help "The rampup period (in seconds) for the script"
     )

instance HasOption NThreads where
  parseOption = nthreadParser

nthreadParser :: Parser NThreads
nthreadParser = NThreads
  <$> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

class Parseable a where
  type family (Parse a)
  buildParser :: Parser a -> Parse a

instance (HasOption a, Parseable api) => Parseable (a -> api) where
  type Parse (a -> api) = Parse api
  buildParser p = z
    where
      y = p <*> parseOption
      z = buildParser y
  
instance Parseable (IO a) where
  type Parse (IO a) = Parser (IO a)
  buildParser = id

runItParser :: (Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO a) -> Parser (IO ())
runItParser f = buildParser $ pure $ \b h l c r n -> void $ f b h l c r n
