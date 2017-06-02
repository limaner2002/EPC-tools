{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Plot.Metrics where

import ClassyPrelude as CP
import Plot
import Data.Time
import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as S
import Control.Arrow
import Data.Attoparsec.ByteString.Streaming
import StreamParser
import ParseSystem
import Control.Monad.Trans.Resource
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Control.Monad.Logger

plotSystem :: (MonadLogger m, MonadIO m, MonadThrow m, MonadBase IO m, MonadBaseControl IO m) => LocalTime -> LocalTime -> FilePath -> [(String, String)] -> Int -> String -> m ()
plotSystem start end filePrefix nodePairs column yAxisTitle = do
  tz <- liftIO getCurrentTimeZone
  l <- mapM
    ( \(filePath, nodeName) -> do
        dataPoints <- BSS.readFile >>> BSS.dropWhile (/= toEnum (fromEnum '\n')) >>> BSS.drop 1 >>> parsed (parseLoadAverage column tz defaultCSVSettings) >>> S.dropWhile before >>> S.break after >>> S.toList >>> runResourceT $ filePath
        return (dataPoints, nodeName)
    ) $ fmap toFilePaths nodePairs
  _ <- liftIO $ renderableToFile def "/tmp/cpu.svg" $ toRenderable $ setTitle . setXScale $ plotIt $ fmap sysTuple $ fmap (S.fst' *** id) l
  return ()
  where
    toTuple (LoadAverage ts val) = (ts, val)
    sysTuple (sys, nodeName) = (fmap toTuple sys, nodeName)
    -- between (LoadAverage ts _) = start <= ts && ts <= end
    -- filterVals (vals, nodeName) = (filter between vals, nodeName)
    setTitle = layout_y_axis . laxis_title .~ yAxisTitle
    setYScale = layout_y_axis . laxis_generate .~ scaledAxis def (0, 300)
    setXScale = layout_x_axis . laxis_generate .~ scaledLocalTimeAxis start end
    scaledLocalTimeAxis start end pts = autoTimeAxis (start : end : pts)
    toFilePaths (pfx, node) = (pfx <> "." <> node, node)
    before x = x ^. timeStamp <= start
    after x = x ^. timeStamp >= end

plotSystem' start end filePrefix column yAxisTitle = plotSystem start end filePrefix nodePairs column yAxisTitle
  where
    nodePairs = zip (repeat filePrefix)
        [ "node1873"
        , "node1894"
        , "node1895"
        , "node1896"
        , "node1897"
        , "node1898"
        , "node1899"
        , "node1900"
        ]
