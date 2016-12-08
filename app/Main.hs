{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Execute

import Server
import Network.Wai.Handler.Warp (run)
import Path
import ConfigFile
import Types (JMeterOpts (..))
import Data.Time (getCurrentTime)
import SendMail
import Data.Time

-- main :: IO ()
-- main = do
--   (scriptsDirIn:_) <- getArgs
--   scriptsDir <- parseRelDir $ unpack scriptsDirIn
--   run 8081 (app scriptsDir)

readJMeterOpts :: MonadThrow m => Config -> m JMeterOpts
readJMeterOpts cfg =
  JMeterOpts <$> getVal' "nRuns"
             <*> getVal' "nUsers"
             <*> getVal' "jmxPath"
             <*> getVal' "jmeterPath"
             <*> getVal' "runName"
             <*> getVal' "otherOpts"
  where
    getVal' k = getVal k cfg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFilePath, timestamp] -> do
      cfg <- readConfigFile $ unpack configFilePath
      let eOpts = readJMeterOpts cfg
      case eOpts of
        Left msg -> fail $ show msg
        Right opts -> do
          let mTime = readMay timestamp
          case mTime of
            Nothing -> fail $ "could not read timestamp: " <> show timestamp
            Just time -> schedule time $ runJMeter opts
    _ -> do
      putStrLn "usage: EPC-tools configFilePath utc-timestamp"
      ct <- getCurrentTime
      putStrLn $ "example: EPC-tools /path/to/configfile " <> tshow ct

toTimeOfDay :: Text -> TimeOfDay
toTimeOfDay txt = fromJust $ readMay txt
  where
    -- The value is hardcoded for now so this is safe to use
    fromJust (Just t) = t

checkJobStatusTime :: TimeOfDay
checkJobStatusTime = toTimeOfDay "08:00:00"

sendScheduledInfo :: TimeOfDay
sendScheduledInfo = toTimeOfDay "00:00:00"
