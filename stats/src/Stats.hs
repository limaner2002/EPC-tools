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

dispRuns :: (MonadResource m, MonadLogger m) => OutputMode -> [FilePath] -> m ()
dispRuns mode paths = do
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
    FormatXlsx -> do
      ws <- S.each >>> streamFold wsRowFold $ rows
      liftBase . print $ ws
      logErrorN "XLSX output is still under development."

toAggRows :: Dict -> [AggregateRow]
toAggRows = fmap (uncurry statToAggregateRow) . sortOn (^. _2 . statTime) . mapToList

showBox :: [AggregateRow] -> String
showBox = render . hsep 2 Text.PrettyPrint.Boxes.left . fmap (vcat Text.PrettyPrint.Boxes.left . fmap text) . transpose . ClassyPrelude.cons headers . fmap (toList . aggToRow)

addHeader :: [[String]] -> [[String]]
addHeader tbl = [headers] <> tbl

headers :: IsString s => [s]
headers = ["Label", "# Samples", "Average", "Median", "90% Line", "95% Line", "99% Line", "Min", "Max", "Errors", "Error%"]

aggToRow :: AggregateRow -> [String]
aggToRow agg = [ unpack (agg ^. aggLabel)
               , show (agg ^. aggNSamples)
               , showFFloat (Just 0) (agg ^. aggAverage) mempty
               , showFFloat (Just 0) (agg ^. aggMedian) mempty
               , showFFloat (Just 0) (agg ^. aggNinetyPercentLine) mempty
               , showFFloat (Just 0) (agg ^. aggNinetyFifthPercentLine) mempty
               , showFFloat (Just 0) (agg ^. aggNinetyNinthPercentLine) mempty
               , dispInfinite (agg ^. aggMinVal)
               , show (agg ^. aggMaxVal)
               , show (agg ^. aggErrors)
               , show (agg ^. aggErrorPct)
               ]

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
