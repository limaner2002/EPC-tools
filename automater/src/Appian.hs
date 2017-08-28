{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds            #-}

module Appian where

import ClassyPrelude hiding (throwM, catch)
import Servant.API
import Servant.Client
import Network.HTTP.Client (CookieJar)
import Control.Monad.Catch

newtype Appian a = Appian { unAppian :: CookieJar -> ClientM a }
  deriving Functor

instance Applicative Appian where
  pure = Appian . const . pure
  f <*> x = Appian $ \cj -> unAppian f cj <*> unAppian x cj

instance Monad Appian where
  x >>= g = Appian $ \cj -> do
    x' <- unAppian x cj
    unAppian (g x') cj

instance MonadIO Appian where
  liftIO f = Appian $ \_ -> liftIO f

instance MonadThrow Appian where
  throwM e = Appian $ \_ -> throwM e

instance MonadCatch Appian where
  catch (Appian f) c = Appian $ \cj -> do
    env <- ask
    eRes <- liftIO $ runClientM (f cj) env
    case eRes of
      Left err -> throwM err `catch` \e -> unAppian (c e ) cj
      Right res -> return res
