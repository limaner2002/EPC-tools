{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE KindSignatures #-}

module Tagless where

import ClassyPrelude
-- import Control.Arrow.ListArrow
-- import Control.Arrow.ArrowList
import Control.Arrow
import JSONTree
import JSONTreeLA
import JSONTreeStreaming
import Data.Aeson
import CreateRequest (SaveInto (..))
import Data.Default
import Network.HTTP.Client (Request)
import Network.HTTP.Types.Method
import Network.HTTP.Simple
-- import Lang

class Auto repr where
  dropSelect :: Text -> Int -> repr
  textFill :: Text -> Text -> repr
  check :: Text -> repr
--  visit :: Text -> repr
  clickLink :: Text -> repr
  clickButton :: Text -> repr

data Expr a = Expr a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Expr
  (Expr f) <*> (Expr b) = Expr (f b)

instance Monad Expr where
  (Expr a) >>= f = f a

data Action
  = HRef
    { href :: Text
    , hrMethod :: Text
    , hrBody :: ByteString
    , hrAccept :: Maybe ByteString
    }
  | Update
    { saveInto :: [SaveInto]
    , value :: Value_
    , atomic :: Bool
    , taskId :: Text
    }
  | Req Request
  deriving Show

instance ToJSON Action where
  toJSON (Update s v at _) = object
    [ "saveInto" .= s
    , "value" .= v
    , "atomic" .= at
    ]
  toJSON (HRef t _ _ _) = object
    [ "link" .= t
    ]

data Value_
  = IntVal Int
  | TxtVal Text
  deriving Show

instance ToJSON Value_ where
  toJSON (IntVal n) = object
    [ "#v" .= n
    , ("#t", "int")
    ]
  toJSON (TxtVal v) = object
    [ "#v" .= v
    , ("#t", "string")
    ]

data UI = UI
  { context :: Context
  , uuid :: Text
  , typ :: Text
  , updates :: [Action]
  } deriving Show

instance ToJSON UI where
  toJSON ui = object
    [ "context" .= context ui
    , "updates" .= updates ui
    , "uuid" .= uuid ui
    , "#t" .= typ ui
    ]

data Context = Context
  { ctxTyp :: Text
  , ctxVal :: Text
  } deriving Show

instance ToJSON Context where
  toJSON ctx = object
    [ "#v" .= ctxVal ctx
    , "#t" .= ctxTyp ctx
    ]

instance FromJSON Context where
  parseJSON (Object o) =
    Context <$> o .: "#t"
            <*> o .: "#v"
  parseJSON _ = fail "Expecting object"

defaultUpdate :: Value_ -> Action
defaultUpdate val = Update mempty val False mempty

defaultHref :: Action
defaultHref = HRef mempty mempty mempty Nothing

data Res a = Res { unRes :: ([Result Action], a)
               }

runRes :: Res a -> [Result Action]
runRes (Res (results, _)) = results

instance (Arrow cat, JSONTree cat Value) => Auto (cat Value (Res Value)) where
  dropSelect label selection = proc input -> do
    drop <- fillField IntVal label selection -< input
    returnA -< Res ([drop], input)

  textFill label selection = proc input -> do
    txt <- fillField TxtVal label selection -< input
    returnA -< Res ([txt], input)

instance (Arrow cat, JSONTree cat Value) => Auto (cat (Res Value) (Res Value)) where
  dropSelect label selection = proc input -> do
    (results, v) <- arr unRes -< input
    drop <- fillField IntVal label selection -< v
    returnA -< Res (results <> [drop], v)

  textFill label selection = proc input -> do
    (results, v) <- arr unRes -< input
    txt <- fillField TxtVal label selection -< v
    returnA -< Res (results <> [txt], v)

fillField :: (Arrow cat, JSONTree cat Value) => (a -> Value_) -> Text -> a -> cat Value (Result Action)
fillField f label v = proc input -> do
  sv <- deep (hasKeyValue "label" (==label)) >>> getKeyValue "saveInto" >>> getChildren >>> arr fromJSON -< input
  taskId <- getKeyValue "taskId" >>> arr fromJSON -< input
  returnA -< createUpdate sv taskId
  where
    createUpdate (Success sv) (Success taskId) = Success $ (defaultUpdate $ f v) {saveInto = [SaveInto sv], taskId = taskId}
    createUpdate (Error strA) (Error strB) = Error (strA <> " and " <> strB)
    createUpdate (Error str) _ = Error str
    createUpdate _ (Error str) = Error str

getContext :: (Arrow cat, JSONTree cat Value) => cat (Result [Action], Value) (Result UI)
getContext = proc (actions, v) -> do
  ctx <- getKeyValue "context" >>> arr fromJSON -< v
  uuid <- getKeyValue "uuid" >>> arr fromJSON -< v
  typ <- getKeyValue "#t" >>> arr fromJSON -< v
  returnA -< UI <$> ctx <*> uuid <*> typ <*> actions

runUI :: (Arrow cat, JSONTree cat Value) => cat (Res Value) (Result UI)
runUI = arr unRes >>> arr sequence *** id >>> getContext

