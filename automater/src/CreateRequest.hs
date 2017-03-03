{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module CreateRequest where

import ClassyPrelude hiding (filter)
import Data.Aeson

import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import JSONTreeLA

import Data.Default
import Control.Arrow

data Action a
  = HRef
    { href :: Text
    , hrMethod :: Text
    }
  | Update
    { saveInto :: [SaveInto]
    , value :: Value_ a
    , atomic :: Bool
    , upTyp :: Text
    , taskId :: Int
    } deriving Show

instance ToJSON a => ToJSON (Action a) where
  toJSON (Update s v at typ _) = object
    [ "#v" .= s
    , "value" .= v
    , "atomic" .= at
    , "#t" .= typ
    ]
  toJSON (HRef t _) = object
    [ "link" .= t
    ]

data Value_ a = Value_
  { val :: a
  , typ :: Text
  } deriving Show

instance ToJSON a => ToJSON (Value_ a) where
  toJSON (Value_ v typ) = object
    [ "#v" .= v
    , "#t" .= typ
    ]

newtype SaveInto = SaveInto Text
  deriving Show

instance ToJSON SaveInto where
  toJSON (SaveInto t) = object
    [ "saveInto" .= t]

instance Default Text where     -- This is a quick hack. Better would be to use a newtype wrapper
  def = mempty

instance Default a => Default (Value_ a) where
  def = Value_ def mempty

instance Default a => Default (Action a) where
  def = Update def def False "SaveRequest?list" 0

dropSelect :: (ArrowChoice a, ArrowIf a) => Text -> Int -> a Value (Action Int)
dropSelect label selection = proc input -> do
  sv <- deep_ (hasKeyValue "label" (==label)) >>> getKeyValue "saveInto" >>> arrL getChildren_ -< input
  taskId <- getKeyValue "taskId" >>> arr valToTextMay >>> arr (join . fmap readMay) >>> maybeL -< input
  returnA -< createUpdate sv taskId
  where
    createUpdate (String sv') taskId = (def :: Action Int) {saveInto = [SaveInto sv'], value = createValue, taskId = taskId}
    createUpdate _ _ = def
    createValue = (Value_ selection "int")

text :: (ArrowChoice a, ArrowIf a) => Text -> Text -> a Value (Action Text)
text label tVal = proc input -> do
  taskId <- getKeyValue "taskId" >>> arr valToTextMay >>> arr (join . fmap readMay) >>> maybeL -< input
  sv <- deep_ (hasKeyValue "label" (==label)) >>> getKeyValue "saveInto" >>> arrL getChildren_ -< input
  
  returnA -< createUpdate sv taskId
  where
    createUpdate (String sv') taskId = (def :: Action Text) {saveInto = [SaveInto sv'], value = createValue, taskId = taskId}
    createUpdate _ _ = def
    createValue = (Value_ tVal "string")

openCaseLink :: (ArrowIf cat, ArrowChoice cat) => cat Value (Action Text)
openCaseLink = proc input -> do
  linkItem <- id //> hasKeyValue "#t" (isSuffixOf "LinkedItem") -< input
  res <- id //> hasKeyValue "#v" (=="SPIN Change") -< linkItem
  link <- getKeyValue  "link" >>> getKeyValue "uri" >>> valToText -< linkItem
  mActID <- arr (lastMay . splitElem '/') -< link
  case mActID of
    Nothing -> returnA -< HRef link "GET"
    Just actionID -> do
      actionLink <- arr mkActionLink -< actionID
      unlistA >>> returnA -< fmap (\x -> HRef x "GET") [link, actionLink]
  where
    mkActionLink actionID = baseURL <> actionID
    baseURL = "/suite/api/tempo/open-a-case/action/"

valToText :: (ArrowList cat, ArrowChoice cat) => cat Value Text
valToText = proc input -> do
  mT <- arr valToTextMay -< input
  case mT of
    Nothing -> none >>> returnA -< mT
    Just t -> returnA -< t

valToTextMay :: Value -> Maybe Text
valToTextMay (String t) = Just t
valToTextMay _ = Nothing

fromJSONL :: (ArrowList cat, ArrowChoice cat, FromJSON a) => cat Value a
fromJSONL = proc input -> do
  res <- arr fromJSON -< input
  case res of
    Success a -> returnA -< a
    Error _ -> none >>> returnA -< res

maybeL :: (ArrowList cat, ArrowChoice cat) => cat (Maybe a) a
maybeL = proc input -> do
  case input of
    Nothing -> none >>> returnA -< input
    Just a -> returnA -< a
