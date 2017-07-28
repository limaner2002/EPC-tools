{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scheduler.Google.Server
  ( Servant.Handler
  , DriveFileType (..)
  , DriveFile (..)
  , IsDriveFile
  , toDriveFile
  , Control.Monad.Trans.Except.throwE
  , servantErr400
  , proxyAPI
  , _DriveFile
  , _Root
  , fName
  , fId
  , fType
  , err400
  , ServerSettings (..)
  , DReq
  , reqFile
  , reqCrumbs
  , DriveRequest (..)
  , runServer
  )where

import ClassyPrelude hiding (Handler)
import Servant hiding (err400)
import qualified Servant as SV
import Control.Lens
import Control.Monad.Trans.Except
import Scheduler.Google.Types
import Control.Monad.Trans.Resource (MonadResource)
import Servant.Utils.StaticFiles
import Network.HTTP.Media ((//))
import qualified Data.ByteString.Lazy as BL
import Servant.HTML.Lucid
import Lucid
import Lucid.Base (makeAttribute)

type HomepageAPI = Get '[HTML] (Html ())
type ViewFilesAPI = "browse" :> ReqBody '[JSON] DReq :> Post '[JSON] ([DriveFile DriveFileType], BreadCrumbs (DriveFile DriveFileType))
type DownloadAPI = "download" :> ReqBody '[JSON] DReq :> Post '[JSON] ()
type StaticAPI = "static" :> Raw
type DReq = DriveRequest (DriveFile DriveFileType)

class IsDriveFile a where
  toDriveFile :: MonadResource m => a -> m (DriveFile DriveFileType)

type API = ViewFilesAPI
  :<|> DownloadAPI
  :<|> StaticAPI
  :<|> HomepageAPI

data ServerSettings = ServerSettings
  { _servFetchContents :: DReq -> Handler ([DriveFile DriveFileType], BreadCrumbs (DriveFile DriveFileType))
  , _servDownloadFile :: DReq -> Handler ()
  , _staticPath :: FilePath
  }

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: ServerSettings -> Server API
server (ServerSettings f g path) = f :<|> g
  :<|> serveDirectory path
  :<|> serveHomepage

serveHomepage :: Server HomepageAPI
serveHomepage = return $ html_ $ do
  head_ $ do
    script'_ [language_ "javascript", src_ "static/rts.js"] mempty
    script'_ [language_ "javascript", src_ "static/lib.js"] mempty
    script'_ [language_ "javascript", src_ "static/out.js"] mempty
  body_ mempty
  script'_ [language_ "javascript", src_ "static/runmain.js"] mempty
  where
    script'_ = termWith "script"
    language_ = makeAttribute "language"

servantErr400 msg = throwE $ SV.err400 {errBody = msg}

err400 msg = SV.err400 {errBody = msg}

runServer :: ServerSettings -> Application
runServer settings = serve proxyAPI $ server settings
