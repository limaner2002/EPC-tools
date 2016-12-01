{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Execute

main :: IO ()
main = do
  (timestamp:jmeterPath:jmxPath:nRuns:users:_) <- getArgs  
  let mOpts = ExeOpts <$> readRun nRuns <*> userList <*> pure (unpack jmxPath) <*> pure (unpack jmeterPath)
      userList = fmap NUsers <$> (readMay users :: Maybe [Int])
      mTime = readMay timestamp
  mapM_ (putStrLn . runningMessage) mOpts
  mapM_ (\(time, opts) -> schedule time $ runJMeter opts) $ (,) <$> mTime <*> mOpts
