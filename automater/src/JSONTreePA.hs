{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}

-- This version is MUCH slower than the LA version
module JSONTreePA where

import JSONTree
import ClassyPrelude
import MachineUtils as MU
import Data.Aeson

instance ArrowApply a => JSONTree (ProcessA a) (Event Value) where
  hasKey k = evMap (hasKey_ k) >>> filterJust

  hasKeyValue k v = evMap (hasKeyValue_ k v) >>> filterJust

  getKeyValue k = evMap (getKeyValue_ k) >>> filterJust

  getChildren = evMap getChildren_ >>> MU.fork

    -- Does not seem to terminate when f is never true
  deep f = proc input -> do
    mIn <- evMap Just >>> hold Nothing -< input
    y <- f -< input
    mRes <- evMap Just >>> hold Nothing -< y
    case mIn of
      Nothing -> returnA -< noEvent
      Just _ -> case mRes of
        Nothing -> do
          bChildren <- evMap hasChildren >>> hold False -< input
          case bChildren of
            True -> getChildren >>> deep f -< input
            False -> returnA -< noEvent
        Just v -> returnA -< v <$ y

hasChildren :: Value -> Bool
hasChildren (Object _) = True
hasChildren (Array _) = True
hasChildren _ = False

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

getKeyValue_ :: Text -> Value -> Maybe Value
getKeyValue_ k (Object o) = lookup k o
getKeyValue_ _ _ = Nothing

getChildren_ :: Value -> [Value]
getChildren_ (Object o) = fmap snd $ mapToList o
getChildren_ (Array arr) = toList arr
getChildren_ _ = []

dispVal :: Value -> String
dispVal (Object _) = "Object"
dispVal (Array _) = "Array"
dispVal (String _) = "String"
dispVal (Number _) = "Number"
dispVal (Bool _) = "Bool"
dispVal Null = "Null"
