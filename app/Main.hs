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
            Just time -> schedule time $ batchJMeterScripts opts
    _ -> do
      putStrLn "usage: EPC-tools utc-timestamp configFile1 configFile2 ..."
      ct <- getCurrentTime
      putStrLn $ "example: EPC-tools \"" <> tshow ct <> "\" /path/to/file1 /path/to/file2"

toTimeOfDay :: Text -> TimeOfDay
toTimeOfDay txt = fromJust $ readMay txt
  where
    -- The value is hardcoded for now so this is safe to use
    fromJust (Just t) = t

checkJobStatusTime :: TimeOfDay
checkJobStatusTime = toTimeOfDay "08:00:00"

sendScheduledInfo :: TimeOfDay
sendScheduledInfo = toTimeOfDay "00:00:00"
