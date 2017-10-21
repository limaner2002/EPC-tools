{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Appian where

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

newtype Cookies = Cookies { _unCookies :: [(ByteString, ByteString)] }
  deriving (Show, Semigroup, Monoid)

makeLenses ''Cookies

-- type Appian = AppianT ClientM

newtype AppianT (m :: * -> *) a = AppianT
  { unAppian :: StateT Cookies m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadTrans, MonadLogger, MonadMask, (MonadState Cookies))

instance RunClient m => RunClient (AppianT m) where
  runRequest req = do
    cookies <- get
    let cookieHeader = (mk "Cookie", cookies ^. unCookies . to renderCookieHeader) -- cookies ^.. unCookies . traverse . runFold ((,) <$> Fold (_1 . to mk) <*> Fold _2) & fromList
        userAgentHeader = (mk "User-Agent", _unUserAgent defUserAgent)
    lift $ runRequest
      req { requestHeaders = userAgentHeader `cons` (cookieHeader `cons` requestHeaders req) }
  throwServantError = lift . throwServantError
  catchServantError m c = mkAppianT $ \cs -> execAppianT m cs `catchServantError` \e -> execAppianT (c e) cs

execAppianT :: AppianT m a -> Cookies -> m (a, Cookies)
execAppianT = runStateT . unAppian

mkAppianT :: (Cookies -> m (a, Cookies)) -> AppianT m a
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
