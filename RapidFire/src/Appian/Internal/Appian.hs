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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

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
import Control.Monad.Trans.Compose
import Control.Monad.Time

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

-- type Appian = AppianT (LoggingT ClientM)
-- type AppianT = forall err. AppianT' err

infixr 9 :.:
type (:.:) = ComposeT

deriving instance MonadLogger (f (g m)) => MonadLogger (ComposeT f g m)
deriving instance MonadThrow (f (g m)) => MonadThrow (ComposeT f g m)
deriving instance MonadCatch (f (g m)) => MonadCatch (ComposeT f g m)
deriving instance MonadMask (f (g m)) => MonadMask (ComposeT f g m)
deriving instance MonadBase IO (f (g m)) => MonadBase IO (ComposeT f g m)

newtype AppianET err (m :: * -> *) a = AppianET
  { unAppian :: (ExceptT err :.: StateT AppianState) m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, (MonadState AppianState), MonadError err)

deriving instance MonadThrow m => MonadThrow (AppianET err m)
deriving instance MonadCatch m => MonadCatch (AppianET err m)
deriving instance MonadLogger m => MonadLogger (AppianET err m)

instance MonadRandom m => MonadRandom (AppianET err m) where
  getRandomWord8 = lift getRandomWord8
  getRandomWord16 = lift getRandomWord16
  getRandomWord32 = lift getRandomWord32
  getRandomWord64 = lift getRandomWord64
  getRandomDouble = lift getRandomDouble

  getRandomNByteInteger = lift . getRandomNByteInteger

instance RunClient m => RunClient (AppianET err m) where
  runRequest req = do
    cookies <- use appianCookies
    let cookieHeader = (mk "Cookie", cookies ^. unCookies . to renderCookieHeader)
        userAgentHeader = (mk "User-Agent", _unUserAgent defUserAgent)
    lift $ runRequest
      req { requestHeaders = userAgentHeader `cons` (cookieHeader `cons` requestHeaders req) }
  throwServantError = lift . throwServantError
  catchServantError m c = mkAppianT $ \cs -> execAppianT m cs `catchServantError` \e -> execAppianT (c e) cs

execAppianT :: AppianET err m a -> AppianState -> m (Either err a, AppianState)
execAppianT = runStateT . runExceptT . getComposeT . unAppian

mkAppianT :: (AppianState -> m (Either err a, AppianState)) -> AppianET err m a
mkAppianT = AppianET . ComposeT . ExceptT . StateT

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

usesValue :: Monad m => (Value -> a) -> AppianET err m a
usesValue = uses appianValue

newAppianState :: Bounds -> AppianState
newAppianState = AppianState mempty Null

catchServerError :: (MonadError ServantError m) => AppianET err m a -> (AppianState -> ServantError -> m (Either err a, AppianState)) -> AppianET err m a
catchServerError f g = do
  appianState <- get
  mkAppianT $ \n -> (execAppianT f n) `catchError` (g appianState)

class Monad m => MonadThreadId (m :: * -> *) where
  threadId :: m ThreadId

instance MonadThreadId IO where
  threadId = myThreadId

instance MonadThreadId ClientM where
  threadId = liftIO threadId

instance (MonadThreadId m, MonadTrans t, Monad (t m)) => MonadThreadId (t m) where
  threadId = lift threadId

class Monad m => MonadDelay (m :: * -> *) where
  delay :: Int -> m ()

instance MonadDelay IO where
  delay = threadDelay

instance MonadDelay ClientM where
  delay = liftIO . delay

instance (MonadDelay m, MonadTrans t, Monad (t m)) => MonadDelay (t m) where
  delay = lift . delay

type RapidFire m = (RunClient m, MonadError ServantError m, MonadTime m, MonadLogger m, MonadCatch m, MonadDelay m, MonadRandom m, MonadError ServantError m, MonadThreadId m)
