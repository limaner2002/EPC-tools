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

import Stats.ParseSample
import StreamParser
import Stats.Types
import Stats.Fold

collectRuns :: [FilePath] -> IO Dict
collectRuns paths = foldM collectStats mempty paths

collectStats :: Dict -> FilePath -> IO Dict
collectStats dict path = do
  res <- BSS.readFile
    >>> BSS.dropWhile (/= toEnum (fromEnum '\n'))
    >>> BSS.drop 1
    >>> parsed (parseRow >=> mapM (pure . decodeUtf8) >=> parseHTTPSample $ defaultCSVSettings)
    >>> streamFold (statFold dict)
    >>> runResourceT $ path
  case S.snd' res of
    Left (msg, _) -> fail $ show msg <> " in " <> path
    Right _ -> return $ S.fst' res

dispRuns :: [FilePath] -> IO ()
dispRuns paths = do
  putStrLn "Gathering stats from:"
  mapM_ (putStrLn . pack) paths
  res <- collectRuns paths
  printBox . hsep 2 Text.PrettyPrint.Boxes.left . fmap (vcat Text.PrettyPrint.Boxes.left . fmap text) . transpose $ addHeader $ fmap (uncurry statToRow) $ sortOn (^. _2 . statTime) $ mapToList res

addHeader :: [[String]] -> [[String]]
addHeader tbl = [["Label", "# Samples", "Average", "Median", "90% Line", "95% Line", "99% Line", "Min", "Max", "Errors", "Error%"]] <> tbl

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
