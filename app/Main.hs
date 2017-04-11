{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Server
import SendMail
import Results
import Scheduler.Server
import Scheduler.Types (emptyQueue)
import Control.Arrow (Kleisli (..))
import GetLogs (downloadLogs)

import Validate ( validateBatchOpts,
                  ValidationResult (..)
                )

import Sheets.Opts

instance MonadThrow ReadM where
  throwM exc = readerError $ show exc

logsParser :: Parser (IO ())
logsParser = downloadLogs <$>
  ( LogSettings
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
  <*> option (parseMany >>= sequence . fmap parseRelFile >>= pure . Just)
  (  long "logfiles"
  <> short 'l'
  <> help "A list of logfiles to download. If more than one node is listed, the list must be separated by spaces and enclosed in double quotes\""
  <> metavar "\"LOGFILE1 <LOGFILE2 ...>\""
  )
  <*> option (readerAsk >>= parseRelDir >>= pure . Just)
  (  long "dest"
  <> short 'd'
  <> help "The directory to download the logfiles to."
  <> metavar "DEST_DIR"
  )
  <*> txtOption
  (  long "url"
  <> short 'a'
  <> help "The url of the Appian environment to download the logs from."
  <> metavar "APPIAN_ENVIRONMENT_ADDRESS"
  ))

logsInfo :: ParserInfo (IO ())
logsInfo = info (helper <*> logsParser)
  (  fullDesc
  <> header "Log Downloader Command-Line"
  <> progDesc "A tool to download log files from an Appian Cloud environment."
  )

txtOption = option (readerAsk >>= pure . pack)

serverParser :: Parser (IO ())
serverParser = runServer
  <$> option (readerAsk >>= parseRelDir)
  (  long "scriptsdir"
  <> short 's'
  <> help "The path where the java scripts are located."
  )

serverInfo :: ParserInfo (IO ())
serverInfo = info (helper <*> serverParser)
  (  fullDesc
  <> header "Log Downloader Server"
  <> progDesc "Runs a simple webserver for automatically downloading log files from Appian Cloud environments."
  )

testsParser :: Parser (IO ())
testsParser = runTests
  <$> flag Execute DryRun
  (  long "dryRun"
  <> short 'd'
  <> help "Run the tests with only 1 user and 1 loop"
  )
  <*> parseToScheduledTime
  <*> option parseMany
   (  short 'c'
   <> help "list of config files to read"
   <> metavar "\"CONFIG1 <CONFIG2 ...>\""
   )

testsInfo :: ParserInfo (IO ())
testsInfo = info (helper <*> testsParser)
  (  fullDesc
  <> header "Run Performance Tests"
  <> progDesc "Runs the performance tests at a specified time."
  )

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

runServer :: Path Rel Dir -> IO ()
runServer scriptsDir = run 8081 (app scriptsDir)

parseCommands :: Parser (IO ())
parseCommands = subparser
  ( command "server" serverInfo
  <> command "run-tests" testsInfo
  <> command "analyse" resultsInfo
  <> command "download-log" logsInfo
  <> command "create-sheet" sheetsInfo
  )

commandsInfo :: ParserInfo (IO ())
commandsInfo = info (helper <*> parseCommands)
  (  fullDesc
  <> progDesc "This is a collection of various tools to help aid in EPC Post Commit performance testing."
  )

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  join $ execParser commandsInfo

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

runTests :: ExecuteType -> ToScheduledTime -> [FilePath] -> IO ()
runTests execType timestamp configFiles = do
  tz <- getCurrentTimeZone
  cfgs <- mapM (readConfigFile . unpack) configFiles
  let eOpts = sequence $ fmap (readJMeterOpts execType) cfgs
  case eOpts of
    Left msg -> fail $ show msg
    Right opts -> do
      ct <- getCurrentTime
      let time' = mkScheduledTime tz timestamp ct
          sendScheduledTime = mkScheduledTime tz sendScheduledInfo ct
          checkJobStatusTime' = mkScheduledTime tz checkJobStatusTime ct
      initialize tz time' sendScheduledTime checkJobStatusTime' opts

initialize :: TimeZone -> ScheduledTime -> ScheduledTime -> ScheduledTime -> [JMeterOpts] -> IO ()
initialize tz jobTime sendScheduledTime checkJobStatusTime opts = do
  (validatedOpts, vr) <- runStateT (validateBatchOpts $ createBatchOpts opts) mempty
  let dispMsg (rn, msg) = show rn <> "\n" <> msg <> "\n\n"
  print vr
  mapM_ (putStr . pack . dispMsg) $ valMsgs vr
  SIO.hFlush stdout
  runningStatus <- newTVarIO NotStarted
  _ <- concurrently
    ( do
        schedule (trace ("scheduledMsg: " <> show sendScheduledTime) sendScheduledTime) $ do
          putStrLn "Sending message now"
          sendMessage $ scheduledMessage validatedOpts sendScheduledTime
        schedule (trace ("checkMsg: " <> show checkJobStatusTime) checkJobStatusTime) $ checkStatus tz runningStatus validatedOpts
    )
    (
      doIfDirIsEmpty $ do
        atomically $ writeTVar runningStatus Running
        sendMessage $ scheduledMessage validatedOpts jobTime
        schedule jobTime $ batchJMeterScripts $ validatedOpts
        atomically $ writeTVar runningStatus Finished
        clt <- utcToLocalTime tz <$> getCurrentTime
        sendMessage $ jobsCompletedMessage clt
    )
  return ()

checkStatus :: TimeZone -> TVar ExecutionStatus -> BatchOpts Validated -> IO ()
checkStatus tz runningStatus opts = go
  where
    go = do
      putStrLn "Sending message now"
      SIO.hFlush stdout
      isRunning <- atomically $ readTVar runningStatus
      clt <- utcToLocalTime tz <$> getCurrentTime
      case isRunning of
        Running -> do
          sendMessage $ stillRunningMessage opts clt
          threadDelay 600000000
          go
        Finished -> sendMessage $ nothingScheduledMessage clt
        NotStarted -> do
          sendMessage $ notStartedMessage clt
          threadDelay 600000000
          go

toTimeOfDay :: Text -> TimeOfDay
toTimeOfDay txt = fromJust $ readMay txt
  where
    -- The value is hardcoded for now so this is safe to use
    fromJust (Just t) = t

checkJobStatusTime :: ToScheduledTime
checkJobStatusTime = TOD $ toTimeOfDay "21:00:00"

sendScheduledInfo :: ToScheduledTime
sendScheduledInfo = TOD $ toTimeOfDay "08:00:00"

toJMeterOpts :: (MonadThrow m, MonadIO m) => FilePath -> m JMeterOpts
toJMeterOpts fp = do
  cfg <- readConfigFile fp
  readJMeterOpts Execute cfg

runServer' :: IO ()
runServer' = do
  v <- newTVarIO emptyQueue
  run 8080 $ application (toJMeterOpts . unpack) (Kleisli (lift . runJMeter)) v
