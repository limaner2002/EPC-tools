{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Appian.Instances where

import ClassyPrelude
import Control.Lens hiding (cons, (.=))
import Servant.API
import Servant.Client hiding (responseStatus)
import Servant.Client.Core
import Control.Monad.Logger
import Control.Monad.State
import Data.Aeson
import Network.HTTP.Media ((//), (/:))
import Data.CaseInsensitive
import qualified Web.Cookie as WC
import Appian
import Appian.Types
import Data.Proxy
import qualified Data.Csv as Csv
import Control.Monad.Time
import Data.Random.Source

data Login = Login
  { _username :: Text
  , _password :: Text
  } deriving (Eq, Show, Ord)

makeLenses ''Login

instance Csv.FromNamedRecord Login where
  parseNamedRecord r = Login
    <$> r Csv..: "username"
    <*> r Csv..: "password"

instance Csv.ToRecord Login where
  toRecord login = Csv.record
    [ Csv.toField $ login ^. username
    , Csv.toField $ login ^. password
    ]

instance Csv.ToNamedRecord Login where
  toNamedRecord login = Csv.namedRecord
    [ ("username", login ^. username . to Csv.toField)
    , ("password", login ^. password . to Csv.toField)
    ]

data AppianConf = AppianConf
  { confCookies :: Cookies
  , confUserAgent :: UserAgent
  } deriving Show

instance RunClient m => RunClient (LoggingT m) where
  runRequest = lift . runRequest
  throwServantError = lift . throwServantError
  catchServantError m c = LoggingT $ \logFun -> runLoggingT m logFun `catchServantError` \e -> runLoggingT (c e) logFun

instance MonadRandom m => MonadRandom (LoggingT m) where
  getRandomWord8 = lift getRandomWord8
  getRandomWord16 = lift getRandomWord16
  getRandomWord32 = lift getRandomWord32
  getRandomWord64 = lift getRandomWord64
  getRandomDouble = lift getRandomDouble

  getRandomNByteInteger = lift . getRandomNByteInteger

instance MonadRandom ClientM where
  getRandomWord8 = liftIO getRandomWord8
  getRandomWord16 = liftIO getRandomWord16
  getRandomWord32 = liftIO getRandomWord32
  getRandomWord64 = liftIO getRandomWord64
  getRandomDouble = liftIO getRandomDouble

  getRandomNByteInteger = liftIO . getRandomNByteInteger

data HTML

instance MimeUnrender HTML ByteString where
  mimeUnrender _ = pure . toStrict

instance Accept HTML where
  contentType _ = "text" // "html"

data AppianApplication

instance FromJSON a => MimeUnrender AppianApplication a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept AppianApplication where
  contentType _ = trace "Getting content type!" $ "application" // "vnd.appian.tv.ui+json"

data AtomApplication

instance FromJSON a => MimeUnrender AtomApplication a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept AtomApplication where
  contentType _ = "application" // "atom+json"

newtype RecordId = RecordId Text
  deriving (Show, Eq)

instance ToHttpApiData RecordId where
  toUrlPiece (RecordId str) = str

data AppianTV

instance FromJSON a => MimeUnrender AppianTV a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept AppianTV where
  contentType _ = "application" // "vnd.appian.tv+json"

instance ToJSON a => MimeRender AppianTV a where
  mimeRender _ = encode

data AppianTVUI

instance FromJSON a => MimeUnrender AppianTVUI a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept AppianTVUI where
  contentType _ = "application" // "vnd.appian.tv.ui+json"

newtype ReportId = ReportId Text
  deriving (Show, Eq)

instance ToHttpApiData ReportId where
  toUrlPiece (ReportId id) = id

newtype TaskId = TaskId Text
  deriving (Show, Eq)

instance ToHttpApiData TaskId where
  toUrlPiece (TaskId id) = id

newtype TTaskId = TTaskId TaskId
  deriving (Show, Eq)

instance ToHttpApiData TTaskId where
  toUrlPiece (TTaskId tid) = "t-" <> toUrlPiece tid

newtype ActionId = ActionId Text
  deriving (Show, Eq)

instance ToHttpApiData ActionId where
  toUrlPiece (ActionId id) = id

newtype ProcessModelId = ProcessModelId Int
  deriving (Show, Eq)

instance ToHttpApiData ProcessModelId where
  toUrlPiece (ProcessModelId id) = toUrlPiece id

instance FromJSON ProcessModelId where
  parseJSON (Object o) = ProcessModelId <$> o .: "processModelId"

instance ToJSON ProcessModelId where
  toJSON (ProcessModelId id) = object
    [ "processModelId" .= id
    ]

data EmptyAppianTV = EmptyAppianTV

instance Accept EmptyAppianTV where
  contentType _ = contentType (Proxy :: Proxy AppianTV)

instance MimeRender EmptyAppianTV EmptyAppianTV where
  mimeRender _ _ = "{}"

instance ToHttpApiData Dashboard where
  toUrlPiece (Dashboard dshbd) = dshbd

data InlineSail

instance FromJSON a => MimeUnrender InlineSail a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept InlineSail where
  contentType _ = contentType (Proxy :: Proxy AtomApplication) /: ("inlineSail", "true")

instance ToHttpApiData RecordRef where
  toUrlPiece (RecordRef ref) = toUrlPiece ref

instance MonadTime ClientM where
  currentTime = liftIO currentTime

instance Csv.FromField AppianUsername where
  parseField bs = AppianUsername <$> Csv.parseField bs

data TrailingSlash

instance (HasClient m api, RunClient m) => HasClient m (TrailingSlash :> api) where
  type Client m (TrailingSlash :> api) = Client m api
  clientWithRoute _ _ req = clientWithRoute (Proxy :: Proxy m) (Proxy :: Proxy api) $ req { requestPath = requestPath req <> "/" }
