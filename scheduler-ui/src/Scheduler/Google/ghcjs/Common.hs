{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Reflex.Dom
import Data.Semigroup
import Data.Text (pack, Text)
import Prelude

head :: MonadWidget t m => m ()
head = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
                ) blank

tshow :: Show a => a -> Text
tshow = pack . show

pureButton :: MonadWidget t m => Text -> m (Event t ())
pureButton label = do
  (e, _) <- elAttr' "a" ("class" =: "pure-button") $ text label
  return $ () <$ domEvent Click e
