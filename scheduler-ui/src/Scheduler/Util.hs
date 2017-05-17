{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Util where

import ClassyPrelude
import System.Directory
import System.FilePath
import Data.Time

getDirNum :: FilePath -> Maybe Int
getDirNum fp = do
  sfx <- lastMay $ splitElem '_' fp
  readMay sfx

getMaxDirNum :: [FilePath] -> Maybe Int
getMaxDirNum paths = case maxNum of
  [Just n] -> Just n
  _ -> Nothing
  where
    maxNum = take 1 . reverse . sort . fmap getDirNum $ paths

createDirectoryIncrement :: FilePath -> IO FilePath
createDirectoryIncrement fp = do
  exists <- doesDirectoryExist fp
  case exists of
    False -> do
      createDirectory fp
      return fp
    True -> do
      contents <- listDirectory $ getParent fp
      let dir = appendNum fp . getMaxDirNum . filter (isPrefixOf $ takeFileName fp) $ contents
      createDirectory dir
      return dir

appendNum :: FilePath -> Maybe Int -> FilePath
appendNum fp Nothing = fp <> "_1"
appendNum fp (Just n) = fp <> "_" <> show (n + 1)

getParent :: FilePath -> FilePath
getParent = foldl' (</>) mempty . reverse . drop 1 . reverse . splitDirectories

createTodayDirectory :: FilePath -> IO FilePath
createTodayDirectory basePath = do
  d <- localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
  createDirectoryIncrement $ basePath </> show d
