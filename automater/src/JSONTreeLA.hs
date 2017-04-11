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

class JSONTree cat a where
  hasKey :: Arrow cat => Text -> cat a a
  hasKeyValue :: Arrow cat => Text -> (Text -> Bool) -> cat a a
  getChildren :: Arrow cat => cat a a
  getKeyValue :: Arrow cat => Text -> cat a a
  deep :: Arrow cat => cat a a -> cat a a

instance JSONTree LA Value where
  hasKey k = arrL (hasKey_ k)
  hasKeyValue k cmp = arrL (hasKeyValue_ k cmp)
  getChildren = arrL getChildren_
  getKeyValue k = arrL (getKeyValue_ k)
  deep = deep_

-- hasKey :: ArrowList a => Text -> a Value Value
-- hasKey k = arrL (hasKey_ k)

hasKey_ :: Text -> Value -> [Value]
hasKey_ k obj@(Object o) = case lookup k o of
  Nothing -> []
  Just _ -> [obj]
hasKey_ _ _ = []

-- hasKeyValue :: ArrowList a => Text -> (Text -> Bool) -> a Value Value
-- hasKeyValue k cmp = arrL (hasKeyValue_ k cmp)

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

-- getKeyValue :: ArrowList a => Text -> a Value Value
-- getKeyValue k = arrL (getKeyValue_ k)

getKeyValue_ :: Text -> Value -> [Value]
getKeyValue_ k (Object o) = case lookup k o of
  Nothing -> []
  Just v -> [v]
getKeyValue_ _ _ = []

(//>) :: ArrowIf cat => cat a Value -> cat Value Value -> cat a Value
(//>) a b = a >>> deep_ b

infixl 5 //>

(/>) :: ArrowIf cat => cat a Value -> cat Value Value -> cat a Value
(/>) a b = a >>> arrL getChildren_ >>> b

infixl 5 />

deep_ :: ArrowIf a => a Value Value -> a Value Value
deep_ f = f
             `orElse`
             (arrL getChildren_ >>> deep_ f)

-- data Value' k v = Value' Value
--   deriving Show

-- data T a b = T a b b

-- class HasKey a where
--   getKey :: a -> a

-- instance HasKey ((,) a a) where
--   getKey (k, _) = k

-- instance HasKey a => Tree (T a) where
--   mkTree x l = _
