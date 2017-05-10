{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}

module StreamingLang where

import ClassyPrelude
import Control.Arrow
import Tagless hiding (fillField)
import JSONTree
import JSONTreeStreaming
import qualified Streaming.Prelude as S
import Data.Aeson
import CreateRequest (SaveInto (..))

instance (Monad m, JSONTree (->) (StreamOf Value m r)) => Auto ( (StreamOf Value m r) -> (StreamOf (Result Action) m r)) where
  dropSelect label selection = proc input -> do
    drop <- fillField IntVal label selection -< input
    returnA -< drop

  textFill label selection = proc input -> do
    txt <- fillField TxtVal label selection -< input
    returnA -< txt

type StreamOf a = S.Stream (S.Of a)

dropSelectS :: Monad m => Text -> Int -> StreamOf Value m () -> StreamOf (Result Action) m ()
dropSelectS = dropSelect

textFillS :: Monad m => Text -> Text -> StreamOf Value m () -> StreamOf (Result Action) m ()
textFillS = textFill

fillField :: (Monad m, JSONTree (->) (StreamOf Value m r)) => (a -> Value_) -> Text -> a -> (->) (StreamOf Value m r) (StreamOf (Result Action) m r)
fillField f label v = proc input -> do
  sv <- deep (hasKeyValue "label" (==label)) >>> getKeyValue "saveInto" >>> getChildren >>> arr (S.map fromJSON) -< input
  taskId <- getKeyValue "taskId" >>> arr (S.map fromJSON) -< input
  returnA -< S.map (uncurry createUpdate) $ S.zip sv taskId
  where
    createUpdate (Success sv) (Success taskId) = Success $ (defaultUpdate $ f v) {saveInto = [SaveInto sv], taskId = taskId}
    createUpdate (Error strA) (Error strB) = Error (strA <> " and " <> strB)
    createUpdate (Error str) _ = Error str
    createUpdate _ (Error str) = Error str
