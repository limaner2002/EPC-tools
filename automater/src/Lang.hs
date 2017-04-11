{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang where

import ClassyPrelude
import Network.HTTP.Client
import Network.HTTP.Types.Method
import Network.HTTP.Simple

class Visit a b | b -> a where
  visit :: MonadThrow m => a -> Automater m b

instance Visit String Request where
  visit url = Automater $ do
    base <- ask
    parseRequest $ base <> url

newtype Automater m a = Automater {unAutomater :: ReaderT String m a}
  deriving Functor

instance Applicative m => Applicative (Automater m) where
  pure = Automater . pure

  (Automater f) <*> (Automater x) = Automater (f <*> x)

instance Monad m => Monad (Automater m) where
  (Automater x) >>= f = Automater $ do
    y <- x
    z <- unAutomater (f y)
    return z

instance MonadTrans Automater where
  lift = Automater . lift

runAutomater :: Automater m a -> String -> m a
runAutomater = runReaderT . unAutomater

setMethod :: StdMethod -> Request -> Request
setMethod meth req = req {method = renderStdMethod meth }

setAccept :: [ByteString] -> Request -> Request
setAccept = setRequestHeader "Accept"
