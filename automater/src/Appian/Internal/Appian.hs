{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE Rank2Types    #-}

module Appian.Internal.Appian where

import ClassyPrelude hiding (throwM, catch)
import Servant.Client
import Network.HTTP.Client (CookieJar)
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.State.Class
import Servant.Client.Core
import Servant.API
import Control.Lens hiding (cons)
import qualified Web.Cookie as WC
import Data.CaseInsensitive
import Data.Aeson
import Data.Random
import Data.Random.Source
import Control.Monad.Except

newtype Cookies = Cookies { _unCookies :: [(ByteString, ByteString)] }
  deriving (Show, Semigroup, Monoid)

makeLenses ''Cookies

data Bounds = Bounds
    { _lowerBound :: Int
    , _upperBound :: Int
    } deriving (Show, Eq, Ord)
    
makeLenses ''Bounds

data AppianState = AppianState
  { _appianCookies :: Cookies
  , _appianValue :: Value
  , _appianBounds :: Bounds
  }

makeLenses ''AppianState

newtype NThreads = NThreads Int
    deriving (Show, Eq, Ord, Num)

type Appian = AppianT (LoggingT ClientM)

newtype AppianT (m :: * -> *) a = AppianT
  { unAppian :: StateT AppianState m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadTrans, MonadLogger, MonadMask, (MonadState AppianState), MonadError e)

deriving instance MonadBase IO m => MonadBase IO (AppianT m)

instance MonadRandom m => MonadRandom (AppianT m) where
  getRandomWord8 = lift getRandomWord8
  getRandomWord16 = lift getRandomWord16
  getRandomWord32 = lift getRandomWord32
  getRandomWord64 = lift getRandomWord64
  getRandomDouble = lift getRandomDouble

  getRandomNByteInteger = lift . getRandomNByteInteger

instance RunClient m => RunClient (AppianT m) where
  runRequest req = do
    cookies <- use appianCookies
    let cookieHeader = (mk "Cookie", cookies ^. unCookies . to renderCookieHeader)
        userAgentHeader = (mk "User-Agent", _unUserAgent defUserAgent)
    lift $ runRequest
      req { requestHeaders = userAgentHeader `cons` (cookieHeader `cons` requestHeaders req) }
  throwServantError = lift . throwServantError
  catchServantError m c = mkAppianT $ \cs -> execAppianT m cs `catchServantError` \e -> execAppianT (c e) cs

execAppianT :: AppianT m a -> AppianState -> m (a, AppianState)
execAppianT = runStateT . unAppian

mkAppianT :: (AppianState -> m (a, AppianState)) -> AppianT m a
mkAppianT = AppianT . StateT

newtype UserAgent = UserAgent
  { _unUserAgent :: ByteString
  } deriving Show

instance ToHttpApiData UserAgent where
  toHeader (UserAgent agentStr) = agentStr

renderCookieHeader :: WC.Cookies -> ByteString
renderCookieHeader = toStrict . builderToLazy . WC.renderCookies

instance ToHttpApiData Cookies where
  toHeader (Cookies cookies) = renderCookieHeader cookies

defUserAgent :: UserAgent
defUserAgent = UserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36"

usesValue :: Monad m => (Value -> a) -> AppianT m a
usesValue = uses appianValue

newAppianState :: Bounds -> AppianState
newAppianState = AppianState mempty Null
