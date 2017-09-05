{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Appian.Instances where

import ClassyPrelude
import Servant.Client hiding (responseStatus)
import Servant
import Control.Lens hiding ((.=))
import qualified Web.Cookie as WC
import Network.HTTP.Client ( newManager, defaultManagerSettings, managerModifyRequest
                           , managerModifyResponse, responseStatus, responseHeaders
                           , responseCookieJar, CookieJar, destroyCookieJar, cookie_name
                           , cookie_value, RequestBody (..)
                           )
import Servant.Common.Req (Req (..), performRequest, performRequestNoBody)
import Data.Aeson
import Network.HTTP.Media ((//), (/:))
import Appian.Types

data Login = Login
  { _username :: Text
  , _password :: Text
  } deriving (Eq, Show)

makeLenses ''Login

newtype LoginCj = LoginCj CookieJar
  deriving (Show, Eq, Read, Monoid)

instance HasClient api => HasClient (Login :> api) where
  type Client (Login :> api) = Login -> Client api

  clientWithRoute _ req login = clientWithRoute (Proxy :: Proxy api)
    $ req { qs = ("un", login ^? username) : ("pw", login ^? password) : qs req
          , headers = ("Content-Type", "application/x-www-form-urlencoded") : headers req <> baseHeaders
          }

instance {-# OVERLAPPING #-} ReflectMethod method => HasClient (Verb method status cts CookieJar) where
  type Client (Verb method status cts CookieJar) = ClientM CookieJar

  clientWithRoute Proxy req = do
    let method = reflectMethod (Proxy :: Proxy method)
    (_, _, _, _, resp) <- performRequest method req
    return $ responseCookieJar resp

instance HasClient api => HasClient (CookieJar :> api) where
  type Client (CookieJar :> api) = CookieJar -> Client api

  clientWithRoute _ req cj = clientWithRoute (Proxy :: Proxy api)
    $ req { headers = headers req <> cookies <> baseHeaders
          }
      where
        renderedCookies = cj ^.. to (destroyCookieJar) . traverse . runFold ((,) <$> Fold (to cookie_name) <*> Fold (to cookie_value)) & toStrict . decodeUtf8 . builderToLazy . WC.renderCookies
        cookies = [("Cookie", renderedCookies)]

instance HasClient api => HasClient (LoginCj :> api) where
  type Client (LoginCj :> api) = LoginCj -> Client api

  clientWithRoute _ req (LoginCj cj) = clientWithRoute (Proxy :: Proxy api)
    $ req { headers = headers req <> cookies <> baseHeaders
          , qs = ("X-APPIAN-CSRF-TOKEN", cj ^? csrfToken) : qs req
          }
      where
        renderedCookies = cj ^.. to (destroyCookieJar) . traverse . runFold ((,) <$> Fold (to cookie_name) <*> Fold (to cookie_value)) & toStrict . decodeUtf8 . builderToLazy . WC.renderCookies
        cookies = [("Cookie", renderedCookies)]

baseHeaders :: [(String, Text)]
baseHeaders =
  [ ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36")
  ]

csrfToken :: (Applicative f, Contravariant f) => (Text -> f Text) -> CookieJar -> f CookieJar
csrfToken = to destroyCookieJar . traverse . filtered (\c -> cookie_name c == "__appianCsrfToken") . to (decodeUtf8 . cookie_value)

data HTML

instance MimeUnrender HTML ByteString where
  mimeUnrender _ = pure . toStrict

instance Accept HTML where
  contentType _ = "text" // "html"

data AppianApplication

instance FromJSON a => MimeUnrender AppianApplication a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept AppianApplication where
  contentType _ = "application" // "vnd.appian.tv.ui+json"

data AtomApplication

instance FromJSON a => MimeUnrender AtomApplication a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept AtomApplication where
  contentType _ = "application" // "atom+json"

data InlineSail

instance FromJSON a => MimeUnrender InlineSail a where
  mimeUnrender _ bs = eitherDecode bs

instance Accept InlineSail where
  contentType _ = contentType (Proxy :: Proxy AtomApplication) /: ("inlineSail", "true")

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

instance ToJSON a => MimeRender AppianTVUI a where
  mimeRender _ = encode

newtype RecordId = RecordId String
  deriving (Show, Eq)

instance HasClient api => HasClient (RecordId :> api) where
  type Client (RecordId :> api) = RecordId -> Client api

  clientWithRoute _ req (RecordId recordId) = clientWithRoute (Proxy :: Proxy api)
    $ req { reqPath = reqPath req <> "/" <> recordId}

newtype RecordRef = RecordRef Text
  deriving (Show, Eq)

instance ToHttpApiData RecordRef where
  toUrlPiece (RecordRef str) = str

newtype TaskId = TaskId Text
  deriving (Show, Eq)

instance ToHttpApiData TaskId where
  toUrlPiece (TaskId id) = id

newtype ReportId = ReportId Text
  deriving (Show, Eq)

instance ToHttpApiData ReportId where
  toUrlPiece (ReportId id) = id

data PathPiece a = PathPiece a
  deriving (Show, Eq)

instance ToHttpApiData a => ToHttpApiData (PathPiece a) where
  toUrlPiece (PathPiece a) = toUrlPiece a

instance (HasClient api, ToHttpApiData a) => HasClient (PathPiece a :> api) where
  type Client (PathPiece a :> api) = PathPiece a -> Client api

  clientWithRoute _ req (PathPiece piece) = clientWithRoute (Proxy :: Proxy api)
    $ req { reqPath = reqPath req <> "/" <> unpack (toUrlPiece piece) }

data Debug = Debug

instance ReflectMethod method => HasClient (Verb method status cts b :> Debug) where
  type Client (Verb method status cts b :> Debug) = Req
  clientWithRoute Proxy req = req

data TaskParams

instance HasClient api => HasClient (TaskParams :> api) where
  type Client (TaskParams :> api) = Client api

  clientWithRoute _ req = clientWithRoute (Proxy :: Proxy api)
    $ req { reqPath = reqPath req <> "/tempo"
          , qs = [ ("m", Just "menu-tasks")
                 , ("t", Just "t")
                 , ("s", Just "pt")
                 , ("defaultFacets", Just "%5Bstatus-open%5D")
                 ]
          }

instance ToHttpApiData TaskParams where
  toQueryParam _ = "tempo?m=menu-tasks&t=t&s=pt&defaultFacets=%255Bstatus-open%255D"

data AppianCsrfToken

instance HasClient api => HasClient (AppianCsrfToken :> api) where
  type Client (AppianCsrfToken :> api) = CookieJar -> Client api

  clientWithRoute _ req cj = clientWithRoute (Proxy :: Proxy api)
    $ req { headers = headers req <> [("X-APPIAN-CSRF-TOKEN", cj ^. csrfToken)] <> cookies }
      where
        renderedCookies = cj ^.. to (destroyCookieJar) . traverse . runFold ((,) <$> Fold (to cookie_name) <*> Fold (to cookie_value)) & toStrict . decodeUtf8 . builderToLazy . WC.renderCookies
        cookies = [("Cookie", renderedCookies)]

newtype NextLink = NextLink Text

data Url

instance HasClient api => HasClient (Url :> api) where
  type Client (Url :> api) = Text -> Client api

  clientWithRoute _ req url = clientWithRoute (Proxy :: Proxy api)
    $ req { reqPath = unpack url }

data EmptyAppianTV

instance HasClient api => HasClient (EmptyAppianTV :> api) where
  type Client (EmptyAppianTV :> api) = Client api

  clientWithRoute _ req = clientWithRoute (Proxy :: Proxy api)
    $ req { reqBody = Just (RequestBodyBS "{}", contentType (Proxy :: Proxy AppianTV)) }

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

instance ToHttpApiData Dashboard where
  toUrlPiece (Dashboard dshbd) = dshbd
