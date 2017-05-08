{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module JSONTreeStreaming where

import ClassyPrelude
import qualified Streaming.Prelude as S
import Data.Aeson
import Control.Arrow
import JSONTree

instance Monad m => JSONTree (->) (S.Stream (S.Of Value) m ()) where
  hasKey k = S.mapMaybe (hasKey_ k)
  hasKeyValue k f = S.mapMaybe (hasKeyValue_ k f)
  getChildren = S.map getChildren_ >>> S.concat
  getKeyValue k = S.mapMaybe (getKeyValue_ k)
  deep = deep_

hasKey_ :: Text -> Value -> Maybe Value
hasKey_ k obj@(Object o) = case lookup k o of
  Nothing -> Nothing
  Just _ -> Just obj
hasKey_ _ _ = Nothing

hasKeyValue_ :: Text -> (Text -> Bool) -> Value -> Maybe Value
hasKeyValue_ k cmp val@(Object o) = lookItUp
  where
    lookItUp = do
      v <- lookup k o
      case v of
        String txt ->
          case cmp txt of
            True -> Just val
            False -> Nothing
        _ -> Nothing
hasKeyValue_ _ _ _ = Nothing

getKeyValue_ :: Text -> Value -> Maybe Value
getKeyValue_ k (Object o) = lookup k o
getKeyValue_ _ _ = Nothing

getChildren_ :: Value -> [Value]
getChildren_ val@(Object o) = fmap snd $ mapToList o
getChildren_ val@(Array arr) = toList arr
getChildren_ _ = []

deep_ :: Monad m => (S.Stream (S.Of Value) m () -> S.Stream (S.Of Value) m ()) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of Value) m ()
deep_ f strm = f strm `orElse` continue
  where
    continue = do
      ev <- lift $ S.next $ getChildren strm
      case ev of
        Left _ -> return ()
        Right (v, resume) -> deep_ f (S.yield v `mappend` resume)

orElse :: Monad m => S.Stream (S.Of a) m r -> S.Stream (S.Of a) m r -> S.Stream (S.Of a) m r
orElse f g = do
  emV <- lift $ S.next f
  case emV of
    Left _ -> g
    Right _ -> f

