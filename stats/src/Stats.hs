{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats where

import ClassyPrelude hiding (try)
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource
import Control.Arrow
import Control.Lens
import Text.PrettyPrint.Boxes hiding ((<>), char)
import Data.List (transpose)
import Numeric
import Data.TDigest (median, quantile)
import Control.Monad.Logger

-- import Stats.ParseSample
-- import StreamParser
import Stats.CsvStream
import Data.Csv
import Data.Csv.Builder
import Data.Time.Clock.POSIX

import Data.Default
import Stats.Types
import Stats.Fold
import Sheets.Generic

collectRuns :: (MonadResource m, MonadLogger m) => [FilePath] -> m Dict
collectRuns = foldM collectStats mempty

collectStats :: (MonadLogger m, MonadResource m) => Dict -> FilePath -> m Dict
collectStats dict path = do
  logDebugN $ "Reading raw data from file " <> pack path
  res <- csvStreamByName >>> streamFold (statFold dict) $ path
  return $ S.fst' res

dispRuns :: (MonadResource m, MonadLogger m) => OutputMode -> [FilePath] -> Verbosity -> m ()
dispRuns mode paths verbosity = do
  logInfoN "Reading raw data from .csv files"
  let msg = "Gathering stats from:\n" <> intercalate "\n" (fmap pack paths)
  logDebugN msg
  res <- collectRuns paths
  let rows = fmap (uncurry statToAggregateRow) $ sortOn (^. _2 . statTime) $ mapToList res
      showBox = liftBase . printBox . hsep 2 Text.PrettyPrint.Boxes.left . fmap (vcat Text.PrettyPrint.Boxes.left . fmap text) . transpose . fmap (fmap (unpack . decodeUtf8) . toList . toRecord)
      showCSV = liftBase . putStrLn . toStrict . decodeUtf8 . concatMap (builderToLazy . encodeRecord)

  logInfoN "Displaying aggregate results"
  case mode of
    FormatTable -> showBox rows
    FormatCSV -> do
      liftBase . putStr . toStrict . decodeUtf8 . builderToLazy . encodeHeader . fromList $ headers
      showCSV rows
    FormatXlsx -> error "FormatXlsx not implemented yet"

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

mkAggSheet :: (MonadResource m, MonadLogger m) => Text -> FilePath -> m (Text, Worksheet)
mkAggSheet name fp = do
  logDebugN $ "Reading sheet " <> name <> " from file " <> pack fp
  ws <- csvStreamByName >>> S.map aggRow >>> streamFold wsRowFold $ fp
  return (name, S.fst' ws)
  where
    aggRow :: AggregateRow -> AggregateRow
    aggRow = id

mkXlsx :: (MonadResource m, MonadLogger m) => S.Stream (S.Of (Text, FilePath)) m r -> m Xlsx
mkXlsx = S.mapM (uncurry mkAggSheet) >>> streamFold (xlsxSheetFold def) >=> S.fst' >>> return

mkAggXlsx :: (MonadResource m, MonadLogger m) => [(Text, FilePath)] -> FilePath -> m ()
mkAggXlsx sheets outputPath = do
  ct <- liftBase getPOSIXTime
  logInfoN "Reading aggregate reports from .csv files"
  x <- S.each >>> mkXlsx $ sheets
  logInfoN $ "Writing output to " <> pack outputPath
  writeFile outputPath . toStrict . fromXlsx ct $ x
