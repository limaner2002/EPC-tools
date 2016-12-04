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
      -- let mOpts = ExeOpts <$> readRun nRuns <*> userList <*> pure (unpack jmxPath) <*> pure (unpack jmeterPath)
      --     userList = fmap NUsers <$> (readMay users :: Maybe [Int])
      --     mTime = readMay timestamp
      -- mapM_ (putStrLn . runningMessage) mOpts
      -- mapM_ (\(time, opts) -> schedule time $ runJMeter opts) $ (,) <$> mTime <*> mOpts
    _ -> do
      putStrLn "usage: EPC-tools utc-timestamp configFile1 configFile2 ..."
      ct <- getCurrentTime
      putStrLn $ "example: EPC-tools \"" <> tshow ct <> "\" /path/to/file1 /path/to/file2"
