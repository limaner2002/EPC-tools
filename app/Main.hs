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
             )
import Data.Time (getCurrentTime)
import SendMail
import Data.Time
import qualified System.IO as SIO

import Path
import Network.Wai.Handler.Warp
import Server

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
             <*> getVal' "sleepTime"
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
            Just time -> do
              ct <- getCurrentTime
              let time' = mkScheduledTime tz time ct
                  sendScheduledTime = mkScheduledTime tz sendScheduledInfo ct
                  checkJobStatusTime' = mkScheduledTime tz checkJobStatusTime ct
              initialize tz time' sendScheduledTime checkJobStatusTime' opts
    _ -> do
      putStrLn "usage: EPC-tools utc-timestamp configFile1 configFile2 ..."
      ct <- getCurrentTime
      putStrLn $ "example: EPC-tools \"" <> tshow ct <> "\" /path/to/file1 /path/to/file2"

-- initialize :: TimeZone -> UTCTime -> TimeOfDay -> TimeOfDay -> [JMeterOpts] -> IO ()
-- initialize tz time sendScheduledTime checkJobStatusTime opts = do
initialize :: TimeZone -> ScheduledTime -> ScheduledTime -> ScheduledTime -> [JMeterOpts] -> IO ()
initialize tz jobTime sendScheduledTime checkJobStatusTime opts =
  case validateBatchOpts $ createBatchOpts opts of
    Left exc -> fail $ show exc
    Right validatedOpts -> do
        runningStatus <- newTVarIO NotStarted
        -- let scheduledTime' = toUTCTime sendScheduledTime
        --     checkJobStatusTime' = toUTCTime checkJobStatusTime
        --     toUTCTime = localTimeToUTC tz . toLocalTime
        --     toLocalTime t = LocalTime (localDay $ utcToLocalTime tz time) t
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
