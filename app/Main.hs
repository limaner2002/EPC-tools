{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Execute

import ConfigFile
import Types (JMeterOpts (..))
import Data.Time (getCurrentTime)
import SendMail
import Data.Time
import qualified System.IO as SIO

-- main :: IO ()
-- main = do
--   (scriptsDirIn:_) <- getArgs
--   scriptsDir <- parseRelDir $ unpack scriptsDirIn
--   run 8081 (app scriptsDir)

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
  where
    getVal' k = getVal k cfg
                -- This is a quick and dirty hack. I need to update
                -- hs-config to properly handle strings that contain
                -- '=' characters.
    replaceIt :: [Text] -> [Text]
    replaceIt = fmap (replaceElem '@' '=')

main :: IO ()
main = do
  args <- getArgs
  tz <- getCurrentTimeZone
  case args of
    (timestamp:configFiles) -> do
      cfgs <- mapM (readConfigFile . unpack) configFiles
      let eOpts = sequence $ fmap readJMeterOpts cfgs
      case eOpts of
        Left msg -> fail $ show msg
        Right opts -> do
          let mTime = readMay timestamp
          case mTime of
            Nothing -> fail $ "could not read timestamp: " <> show timestamp
            Just time -> initialize tz time sendScheduledInfo checkJobStatusTime opts
    _ -> do
      putStrLn "usage: EPC-tools utc-timestamp configFile1 configFile2 ..."
      ct <- getCurrentTime
      putStrLn $ "example: EPC-tools \"" <> tshow ct <> "\" /path/to/file1 /path/to/file2"

initialize :: TimeZone -> UTCTime -> TimeOfDay -> TimeOfDay -> [JMeterOpts] -> IO ()
initialize tz time sendScheduledTime checkJobStatusTime opts = do
  runningStatus <- newTVarIO NotStarted
  let scheduledTime' = toUTCTime sendScheduledTime
      checkJobStatusTime' = toUTCTime checkJobStatusTime
      toUTCTime = localTimeToUTC tz . toLocalTime
      toLocalTime t = LocalTime (localDay $ utcToLocalTime tz time) t
  sendMessage $ scheduledMessage (fmap runName opts) (utcToLocalTime tz time)
  _ <- concurrently
    ( do
        schedule (trace ("scheduledMsg: " <> show scheduledTime') scheduledTime') $ do
          putStrLn "Sending message now"
          sendMessage $ scheduledMessage (fmap runName opts) (utcToLocalTime tz time)
        schedule (trace ("checkMsg: " <> show checkJobStatusTime') checkJobStatusTime') $ checkStatus tz runningStatus opts
    )
    ( do
        doIfDirIsEmpty $ schedule time $ do
          atomically $ writeTVar runningStatus Running
          batchJMeterScripts opts
          atomically $ writeTVar runningStatus Finished
    )
  return ()

checkStatus :: TimeZone -> TVar ExecutionStatus -> [JMeterOpts] -> IO ()
checkStatus tz runningStatus opts = go
  where
    go = do
      putStrLn "Sending message now"
      SIO.hFlush stdout
      isRunning <- atomically $ readTVar runningStatus
      clt <- utcToLocalTime tz <$> getCurrentTime
      case isRunning of
        Running -> do
          sendMessage $ stillRunningMessage (fmap runName opts) clt
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

checkJobStatusTime :: TimeOfDay
checkJobStatusTime = toTimeOfDay "21:00:00"

sendScheduledInfo :: TimeOfDay
sendScheduledInfo = toTimeOfDay "08:00:00"
