{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Reflex.Dom
import Data.Semigroup
import Data.Text (pack, Text)
import Prelude
import Data.Map (Map)

head :: MonadWidget t m => m ()
head = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
                ) blank

tshow :: Show a => a -> Text
tshow = pack . show

pureButtonAttr :: MonadWidget t m => Text -> Map Text Text -> m (Event t ())
pureButtonAttr label attrs = do
  (e, _) <- elAttr' "a" attrs $ text label
  return $ () <$ domEvent Click e

pureButton :: MonadWidget t m => Text -> m (Event t ())
pureButton label = pureButtonAttr label ("class" =: "pure-button")

pureButtonPrimary :: MonadWidget t m => Text -> m (Event t ())
pureButtonPrimary label = pureButtonAttr label ("class" =: "pure-button pure-button-primary")
