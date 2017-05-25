{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Logs.Opts where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types
import Logs.GetLogs
import Control.Monad.Catch

logsParser :: Parser (IO ())
logsParser = downloadLogs
  <$> txtOption
  (  long "username"
  <> short 'u'
  <> help "The username to use to login to the Appian environment."
  <> metavar "USERNAME"
  )
  <*> txtOption
  (  long "password"
  <> short 'p'
  <> help "The password of the user to login to the Appian environment."
  <> metavar "PASSWORD"
  )
  <*> option (fmap pack <$> parseMany)
  (  long "nodes"
  <> short 'n'
  <> help "A list of nodes to download the logs from. If more than one node is listed, the list must be separated by spaces and enclosed in double quotes\""
  <> metavar "\"NODE1 <NODE2 ...>\""
  )
  <*> option (runReadThrow $ ReadMThrow parseMany)
  (  long "logfiles"
  <> short 'l'
  <> help "A list of logfiles to download. If more than one node is listed, the list must be separated by spaces and enclosed in double quotes\""
  <> metavar "\"LOGFILE1 <LOGFILE2 ...>\""
  )
  <*> option (runReadThrow $ ReadMThrow readerAsk)
  (  long "dest"
  <> short 'd'
  <> help "The directory to download the logfiles to."
  <> metavar "DEST_DIR"
  )
  <*> strOption
  (  long "url"
  <> short 'a'
  <> help "The url of the Appian environment to download the logs from."
  <> metavar "APPIAN_ENVIRONMENT_ADDRESS"
  )

logsInfo :: ParserInfo (IO ())
logsInfo = info (helper <*> logsParser)
  (  fullDesc
  <> header "Log Downloader Command-Line"
  <> progDesc "A tool to download log files from an Appian Cloud environment."
  )

txtOption :: Mod OptionFields Text -> Parser Text
txtOption = option (readerAsk >>= pure . pack)

parseMany :: ReadM [String]
parseMany = readerAsk >>= pure . words

newtype ReadMThrow a = ReadMThrow {runReadThrow :: ReadM a}
  deriving (Functor, Applicative, Monad)

instance MonadThrow ReadMThrow where
  throwM exc = ReadMThrow $ readerError $ show exc
