{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module JSONTree where

import ClassyPrelude
import Control.Arrow

class JSONTree cat a where
  hasKey :: Arrow cat => Text -> cat a a
  hasKeyValue :: Arrow cat => Text -> (Text -> Bool) -> cat a a
  getChildren :: Arrow cat => cat a a
  getKeyValue :: Arrow cat => Text -> cat a a
  deep :: Arrow cat => cat a a -> cat a a

(//>) :: (JSONTree cat c, Arrow cat) => cat a c -> cat c c -> cat a c
(//>) f g = f >>> deep g

infixl 5 //>

(/>) :: (JSONTree cat b, Arrow cat) => cat a b -> cat b c -> cat a c
(/>) f g = f >>> getChildren >>> g

infixl 5 />
