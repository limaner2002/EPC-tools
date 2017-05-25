{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats where

import ClassyPrelude hiding (try)
import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as S
import Data.Attoparsec.ByteString.Streaming
import Control.Monad.Trans.Resource
import Control.Arrow
import Control.Lens
import Text.PrettyPrint.Boxes hiding ((<>), char)
import Data.List (transpose)
import Numeric
import Data.TDigest (median, quantile)

-- import Stats.ParseSample
-- import StreamParser
import Stats.CsvStream
import Data.Csv
import Data.Csv.Builder

import Data.Default
import Stats.Types
import Stats.Fold

collectRuns :: [FilePath] -> IO Dict
collectRuns paths = foldM collectStats mempty paths

collectStats :: Dict -> FilePath -> IO Dict
collectStats dict path = do
  res <- csvStreamByName HasHeader
    >>> streamFold (statFold dict)
    >>> runResourceT $ path
  return $ S.fst' res

dispRuns :: OutputMode -> [FilePath] -> Verbosity -> IO ()
dispRuns mode paths verbosity = do
  case verbosity of
    Verbose -> do
      putStrLn "Gathering stats from:"
      mapM_ (putStrLn . pack) paths
    Quiet -> return ()
  res <- collectRuns paths
  let rows = fmap (uncurry statToAggregateRow) $ sortOn (^. _2 . statTime) $ mapToList res
      showBox = printBox . hsep 2 Text.PrettyPrint.Boxes.left . fmap (vcat Text.PrettyPrint.Boxes.left . fmap text) . transpose . (fmap . fmap) (unpack . decodeUtf8) . fmap (toList . toRecord)
      showCSV = putStrLn . toStrict . decodeUtf8 . concat . fmap (builderToLazy . encodeRecord)

  case mode of
    Table -> showBox rows
    CSV -> do
      putStr . toStrict . decodeUtf8 . builderToLazy . encodeHeader . fromList $ headers
      showCSV rows

addHeader :: [[String]] -> [[String]]
addHeader tbl = [headers] <> tbl

headers :: IsString s => [s]
headers = ["Label", "# Samples", "Average", "Median", "90% Line", "95% Line", "99% Line", "Min", "Max", "Errors", "Error%"]

statToRow :: Label -> Stat -> [String]
statToRow statLabel stat = [ unpack (statLabel ^. labelVal)
                           , show (stat ^. statTotal)
                           , showFFloat (Just 0) avg mempty
                           , showFFloat (Just 0) (fromMaybe (0/0) $ median dg) mempty
                           , showFFloat (Just 0) (fromMaybe (0/0) $ quantile 0.9 dg) mempty
                           , showFFloat (Just 0) (fromMaybe (0/0) $ quantile 0.95 dg) mempty
                           , showFFloat (Just 0) (fromMaybe (0/0) $ quantile 0.99 dg) mempty
                           , dispInfinite (stat ^. statMin)
                           , show (stat ^. statMax)
                           , show (stat ^. statErrors)
                           , showFFloat (Just 2) pctg mempty
                           ]
  where
    pctg :: Double
    pctg = fromIntegral (stat ^. statErrors) / fromIntegral (stat ^. statTotal) * 100
    avg :: Double
    avg = fromIntegral (stat ^. statElapsed) / fromIntegral (stat ^. statTotal)
    dg = stat ^. statDigest
