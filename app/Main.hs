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
             , validateBatchOpts
             , createBatchOpts
             , BatchOpts
             , Validated
             , LogSettings (..)
             )
import Data.Time
import qualified System.IO as SIO

import Options.Applicative hiding ((<>))
import qualified Options.Applicative as OA
import Options.Applicative.Types
import Control.Monad.Catch
import Control.Monad.Trans.State

import Path
import Network.Wai.Handler.Warp
import Server
import SendMail
import Results
import GetLogs (downloadLogs)

instance MonadThrow ReadM where
  throwM exc = readerError $ show exc

logsParser :: Parser (IO ())
logsParser = downloadLogs <$>
  ( LogSettings
  <$> txtOption
  (  long "username"
  OA.<> short 'u'
  OA.<> help "The username to use to login to the Appian environment."
  OA.<> metavar "USERNAME"
  )
  <*> txtOption
  (  long "password"
  OA.<> short 'p'
  OA.<> help "The password of the user to login to the Appian environment."
  OA.<> metavar "PASSWORD"
  )
  <*> option (fmap pack <$> parseMany)
  (  long "nodes"
  OA.<> short 'n'
  OA.<> help "A list of nodes to download the logs from. If more than one node is listed, the list must be separated by spaces and enclosed in double quotes\""
  OA.<> metavar "\"NODE1 <NODE2 ...>\""
  )
  <*> option (parseMany >>= sequence . fmap parseRelFile >>= pure . Just)
  (  long "logfiles"
  OA.<> short 'l'
  OA.<> help "A list of logfiles to download. If more than one node is listed, the list must be separated by spaces and enclosed in double quotes\""
  OA.<> metavar "\"LOGFILE1 <LOGFILE2 ...>\""
  )
  <*> option (readerAsk >>= parseRelDir >>= pure . Just)
  (  long "dest"
  OA.<> short 'd'
  OA.<> help "The directory to download the logfiles to."
  OA.<> metavar "DEST_DIR"
  )
  <*> txtOption
  (  long "url"
  OA.<> short 'a'
  OA.<> help "The url of the Appian environment to download the logs from."
  OA.<> metavar "APPIAN_ENVIRONMENT_ADDRESS"
  ))

logsInfo :: ParserInfo (IO ())
logsInfo = info (helper <*> logsParser)
  (  fullDesc
  OA.<> header "Log Downloader Command-Line"
  OA.<> progDesc "A tool to download log files from an Appian Cloud environment."
  )

txtOption = option (readerAsk >>= pure . pack)

serverParser :: Parser (IO ())
serverParser = runServer
  <$> option (readerAsk >>= parseRelDir)
  (  long "scriptsdir"
  OA.<> short 's'
  OA.<> help "The path where the java scripts are located."
  )

serverInfo :: ParserInfo (IO ())
serverInfo = info (helper <*> serverParser)
  (  fullDesc
  OA.<> header "Log Downloader Server"
  OA.<> progDesc "Runs a simple webserver for automatically downloading log files from Appian Cloud environments."
  )

testsParser :: Parser (IO ())
testsParser = runTests
  <$> parseToScheduledTime
  <*> option parseMany
   (  short 'c'
   OA.<> help "list of config files to read"
   OA.<> metavar "\"CONFIG1 <CONFIG2 ...>\""
   )

testsInfo :: ParserInfo (IO ())
testsInfo = info (helper <*> testsParser)
  (  fullDesc
  OA.<> header "Run Performance Tests"
  OA.<> progDesc "Runs the performance tests at a specified time."
  )

parseMany :: ReadM [String]
parseMany = readerAsk >>= pure . words

parseToScheduledTime :: Parser ToScheduledTime
parseToScheduledTime =
  At <$> option (readTime' "%X %Z")
  (  long "at"
  OA.<> help "Schedule the tests for a specific time and day."
  )
  <|>
  TOD <$> option (readTime' "%X %Z")
  (  long "tod"
  OA.<> help "Schedule the tests for a specific time of day. If the time of day is before the current time, the tests will be scheduled for the next day at that time."
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
  OA.<> command "run-tests" testsInfo
  OA.<> command "analyse" resultsInfo
  OA.<> command "download-log" logsInfo
  )

commandsInfo :: ParserInfo (IO ())
commandsInfo = info (helper <*> parseCommands)
  (  fullDesc
  OA.<> progDesc "This is a collection of various tools to help aid in EPC Post Commit performance testing."
  )

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  join $ execParser commandsInfo

data ExecutionStatus
  = NotStarted
  | Running
  | Finished

readJMeterOpts :: MonadThrow m => Config -> m JMeterOpts
readJMeterOpts cfg =
  JMeterOpts <$> getVal' "nRuns"
             <*> getVal' "nUsers"
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

runTests :: ToScheduledTime -> [FilePath] -> IO ()
runTests timestamp configFiles = do
  tz <- getCurrentTimeZone
  cfgs <- mapM (readConfigFile . unpack) configFiles
  let eOpts = sequence $ fmap readJMeterOpts cfgs
  case eOpts of
    Left msg -> fail $ show msg
    Right opts -> do
      ct <- getCurrentTime
      let time' = mkScheduledTime tz timestamp ct
          sendScheduledTime = mkScheduledTime tz sendScheduledInfo ct
          checkJobStatusTime' = mkScheduledTime tz checkJobStatusTime ct
      initialize tz time' sendScheduledTime checkJobStatusTime' opts

-- runTests timestamp configFiles = do
--   args <- getArgs
--   tz <- getCurrentTimeZone
--   case args of
--     (timestamp:configFiles) -> do
--       cfgs <- mapM (readConfigFile . unpack) configFiles
--       let eOpts = sequence $ fmap readJMeterOpts cfgs
--       case eOpts of
--         Left msg -> fail $ show msg
--         Right opts -> do
--           let mTime = readMay timestamp
--           case mTime of
--             Nothing -> fail $ "could not read timestamp: " <> show timestamp
--             Just time -> do
--               ct <- getCurrentTime
--               let time' = mkScheduledTime tz time ct
--                   sendScheduledTime = mkScheduledTime tz sendScheduledInfo ct
--                   checkJobStatusTime' = mkScheduledTime tz checkJobStatusTime ct
--               initialize tz time' sendScheduledTime checkJobStatusTime' opts
--     _ -> do
--       putStrLn "usage: EPC-tools utc-timestamp configFile1 configFile2 ..."
--       ct <- getCurrentTime
--       putStrLn $ "example: EPC-tools \"" <> tshow ct <> "\" /path/to/file1 /path/to/file2"

-- initialize :: TimeZone -> UTCTime -> TimeOfDay -> TimeOfDay -> [JMeterOpts] -> IO ()
-- initialize tz time sendScheduledTime checkJobStatusTime opts = do
initialize :: TimeZone -> ScheduledTime -> ScheduledTime -> ScheduledTime -> [JMeterOpts] -> IO ()
initialize tz jobTime sendScheduledTime checkJobStatusTime opts = do
  (validatedOpts, msgs) <- runStateT (validateBatchOpts $ createBatchOpts opts) mempty
  let dispMsg (rn, msg) = show rn <> "\n" <> msg <> "\n\n"
  print msgs
  mapM_ (putStr . pack . dispMsg) msgs
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
