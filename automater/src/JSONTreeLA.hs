{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module JSONTreeLA where

import ClassyPrelude
import Data.Aeson
import Control.Arrow.ArrowList
import Control.Arrow
import Control.Arrow.ArrowIf
import Control.Arrow.ListArrow
import JSONTree

instance JSONTree LA Value where
  hasKey k = arrL (hasKey_ k)
  hasKeyValue k cmp = arrL (hasKeyValue_ k cmp)
  getChildren = arrL getChildren_
  getKeyValue k = arrL (getKeyValue_ k)
  deep = deep_

hasKey_ :: Text -> Value -> [Value]
hasKey_ k obj@(Object o) = case lookup k o of
  Nothing -> []
  Just _ -> [obj]
hasKey_ _ _ = []

hasKeyValue_ :: Text -> (Text -> Bool) -> Value -> [Value]
hasKeyValue_ k cmp val@(Object o) = case lookItUp of
  Nothing -> []
  Just v -> [v]
  where
    lookItUp = do
      v <- lookup k o
      case v of
        String txt ->
          case cmp txt of
            True -> Just val
            False -> Nothing
        _ -> Nothing
hasKeyValue_ _ _ _ = []

getChildren_ :: Value -> [Value]
getChildren_ (Object o) = fmap snd $ mapToList o
getChildren_ (Array arr) = toList arr
getChildren_ _ = []

getKeyValue_ :: Text -> Value -> [Value]
getKeyValue_ k (Object o) = case lookup k o of
  Nothing -> []
  Just v -> [v]
getKeyValue_ _ _ = []

deep_ :: ArrowIf a => a Value Value -> a Value Value
deep_ f = f
             `orElse`
             (arrL getChildren_ >>> deep_ f)
