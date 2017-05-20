{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Control.Foldl as Fold

import Stats.ParseSample
import StreamParser

data Stat = Stat
  { _statTime :: UTCTime
  , _statTotal :: Int
  , _statErrors :: Int
  , _statElapsed :: Int
  } deriving Show

type Dict = Map Label Stat

newStat :: UTCTime -> Stat
newStat ts = Stat ts 0 0 0

makeLenses ''Stat

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

streamFold :: Monad m => Fold.Fold t1 t -> S.Stream (S.Of t1) m r -> m (S.Of t r)
streamFold (Fold.Fold accum init f) = S.fold accum init f

statFold :: Dict -> Fold.Fold HTTPSample Dict
statFold dict = Fold.Fold collectStat dict id

collectStat :: Dict -> HTTPSample -> Dict
collectStat d sample = d & at (sample ^. label) %~ update
  where
    update Nothing = Just $ makeStat (newStat (sample ^. timeStamp)) sample
    update (Just stat) = Just $ makeStat stat sample

makeStat :: Stat -> HTTPSample -> Stat
makeStat stat sample = stat
  & statTotal %~ (+1)
  & statErrors %~ (isError (sample ^. responseCode))
  & statElapsed %~ (\avg -> avg + sample ^. elapsed)
  where
    isError NoResponseCode n = n
    isError respCode n =
      case respCode ^? _HTTPResponseCode of
        Just code -> 
          if code < 400
          then n
          else n + 1
        Nothing -> n + 1

dispRuns :: [FilePath] -> IO ()
dispRuns paths = do
  putStrLn "Gathering stats from:"
  mapM_ (putStrLn . pack) paths
  res <- collectRuns paths
  printBox . hsep 2 Text.PrettyPrint.Boxes.left . fmap (vcat Text.PrettyPrint.Boxes.left . fmap text) . transpose $ fmap (uncurry statToRow) $ sortOn (^. _2 . statTime) $ mapToList res

statToRow :: Label -> Stat -> [String]
statToRow statLabel stat = [ unpack (statLabel ^. labelVal)
                           , show (stat ^. statTotal)
                           , show (stat ^. statErrors)
                           , showFFloat (Just 2) pctg mempty
                           , showFFloat (Just 0) avg mempty
                           ]
  where
    pctg :: Double
    pctg = fromIntegral (stat ^. statErrors) / fromIntegral (stat ^. statTotal) * 100
    avg :: Double
    avg = fromIntegral (stat ^. statElapsed) / fromIntegral (stat ^. statTotal)
