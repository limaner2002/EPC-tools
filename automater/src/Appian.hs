{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds            #-}

module Appian where

import ClassyPrelude hiding (throwM, catch)
import Servant.Client
import Network.HTTP.Client (CookieJar)
import Control.Monad.Catch
import Control.Monad.Logger

type Appian = AppianT ClientM

newtype AppianT (m :: * -> *) a = AppianT
  { unAppian :: CookieJar -> m a
  } deriving Functor

instance Applicative m => Applicative (AppianT m) where
  pure = AppianT . const . pure
  f <*> x = AppianT $ \cj -> unAppian f cj <*> unAppian x cj

instance Monad m => Monad (AppianT m) where
  x >>= g = AppianT $ \cj -> do
    x' <- unAppian x cj
    unAppian (g x') cj

instance MonadIO m => MonadIO (AppianT m) where
  liftIO f = AppianT $ \_ -> liftIO f

instance MonadThrow m => MonadThrow (AppianT m) where
  throwM e = AppianT $ \_ -> throwM e

instance MonadCatch (AppianT ClientM) where
  catch (AppianT f) c = AppianT $ \cj -> do
    env <- ask
    eRes <- liftIO $ runClientM (f cj) env
    case eRes of
      Left err -> throwM err `catch` \e -> unAppian (c e ) cj
      Right res -> return res

instance MonadTrans AppianT where
  lift = AppianT . const

instance MonadLogger m => MonadLogger (AppianT m)
