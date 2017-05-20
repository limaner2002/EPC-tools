{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats.Fold where

import ClassyPrelude
import qualified Control.Foldl as Fold
import Stats.Types
import Data.TDigest
import GHC.TypeLits (KnownNat)
import qualified Streaming.Prelude as S
import Control.Lens

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
  & statDigest %~ (\digest -> insert (fromIntegral $ sample ^. elapsed) digest)
  & statMin %~ (min (Only $ sample ^. elapsed))
  & statMax %~ (max (sample ^. elapsed))
  where
    isError NoResponseCode n = n
    isError respCode n =
      case respCode ^? _HTTPResponseCode of
        Just code -> 
          if code < 400
          then n
          else n + 1
        Nothing -> n + 1

digestFold :: KnownNat comp => Fold.Fold HTTPSample (TDigest comp)
digestFold = Fold.Fold (flip (insert . fromIntegral . view elapsed)) mempty id

