{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module CreateRequest where

import ClassyPrelude hiding (filter)
import Data.Aeson

import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ListArrow (runLA)
import JSONTreeLA

import Data.Default
import Control.Arrow

data Action a
  = HRef
    { href :: Text
    , hrMethod :: Text
    , hrBody :: ByteString
    , hrAccept :: Maybe ByteString
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
  toJSON (HRef t _ _ _) = object
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
    Nothing -> returnA -< HRef link "GET" mempty Nothing
    Just actionID -> do
      actionLink <- arr mkActionLink -< actionID
      unlistA >>> returnA -< fmap (\x -> HRef x "GET" mempty Nothing) [actionLink]
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

getAttrLinks :: (ArrowIf cat, ArrowChoice cat) => cat Value (Action Text)
-- getAttrLinks = id //> hasKey "entries" >>> getKeyValue "entries" /> getKeyValue "links" /> hasKeyValue "rel" (=="task.attributes") >>> getKeyValue "href" >>> valToText >>> arr (\x -> HRef x "GET" mempty (Just "application/atom+json,application/json"))
getAttrLinks = proc input -> do
  taskID <- id //> hasKey "entries" >>> getKeyValue "entries" //> getKeyValue "id" >>> valToText >>> arr (stripPrefix "t-") >>> maybeL >>> arr (baseURL <>) -< input
  attrAct <- arr (<> "/attributes") >>> arr (\x -> mkHRef x "GET" mempty (Just "application/json")) -< taskID
  returnA -< attrAct
  where
    baseURL = "/suite/rest/a/task/latest/"

getAttrLink :: ArrowList cat => cat Value (Action Text)
getAttrLink = arr (runLA getAttrLinks) >>> arrL (take 1)

mkHRef :: Text -> Text -> ByteString -> Maybe ByteString -> Action a
mkHRef url method body mAccept
  | isPrefixOf "http://" url || isPrefixOf "https://" url = HRef url method body mAccept
  | otherwise = HRef (baseUrl <> url) method body mAccept

baseUrl :: IsString t => t
baseUrl = "https://portal-preprod.usac.org"
