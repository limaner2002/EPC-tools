{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Scheduler.Google where

import Control.Lens
import Network.Google
import Network.Google.Drive
import ClassyPrelude
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource hiding (throwM)
import Network.Google.Auth.Scope
import qualified Scheduler.Google.Server as DS
import Control.Monad.Trans.Except hiding (throwM)
import Scheduler.Google.Types hiding (fId, fName)
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing, listDirectory)
import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeDirectory)
import Codec.Archive.Zip

newtype MissingConfigException = MissingConfigException Text
  deriving Show

instance Exception MissingConfigException

g :: MonadResource m => ResumableSource m ByteString -> FilePath -> m ()
g stream fp = stream $$+- CC.sinkFile fp

downloadFile :: (HasScope'
        s
        '["https://www.googleapis.com/auth/drive",
          "https://www.googleapis.com/auth/drive.appdata",
          "https://www.googleapis.com/auth/drive.file",
          "https://www.googleapis.com/auth/drive.metadata",
          "https://www.googleapis.com/auth/drive.metadata.readonly",
          "https://www.googleapis.com/auth/drive.photos.readonly",
          "https://www.googleapis.com/auth/drive.readonly"]
      ~
      'True,
      MonadGoogle s m, MonadThrow m, MonadResource m) => FilePath -> DS.DReq -> m FilePath
downloadFile pathPrefix req = do
  case filesGet <$> df ^? DS.fId of
    Nothing -> throwM $ DS.err400 "No file to download!"
    Just fg -> do
      let dir = pathPrefix </> unpack (intercalate "/" $ req ^.. reqCrumbs . bcCrumbs . traverse . DS.fName)
          fp = dir </> unpack (df ^. DS.fName)
      stream <- download fg
      putStrLn $ "Downloading file " <> tshow (df ^. DS.fName) <> " to " <> tshow fp
      liftIO $ createDirectoryIfMissing True dir
      liftBase $ runResourceT $ g stream fp
      extractFile fp
 where
   df = req ^. reqFile

extractFile :: (MonadBase IO m, MonadThrow m) => FilePath -> m FilePath
extractFile fp = do
  ct <- liftBase $ BL.readFile fp
  let dir = takeDirectory fp
  liftBase $ extractFilesFromArchive [OptDestination dir] . toArchive $ ct
  files <- liftBase $ listDirectory dir
  let mCfg = files ^? traverse . filtered (isSuffixOf ".cfg")
  case mCfg of
    Nothing -> throwM $ MissingConfigException "There is no configuration file! This script cannot be scheduled."
    Just cfgFile -> return $ dir </> cfgFile

epcEnv :: (MonadCatch f, MonadIO f) => f (Env '["https://www.googleapis.com/auth/drive.readonly"])
epcEnv = do
  lgr <- newLogger Debug stdout
  newEnv <&> (envLogger .~ lgr) . (envScopes .~ driveReadOnlyScope)

mkQuery :: Text -> Text
mkQuery id = tshow id <> " in parents"

rootContents :: (HasScope'
        s
        '["https://www.googleapis.com/auth/drive",
          "https://www.googleapis.com/auth/drive.appdata",
          "https://www.googleapis.com/auth/drive.file",
          "https://www.googleapis.com/auth/drive.metadata",
          "https://www.googleapis.com/auth/drive.metadata.readonly",
          "https://www.googleapis.com/auth/drive.photos.readonly",
          "https://www.googleapis.com/auth/drive.readonly"]
      ~
      'True,
      MonadGoogle s m) =>
     m FileList
rootContents = send $ flQ .~ Just "'0BwCCV-kDSmY_eHRLcVhmUmpQdUk' in parents" $ filesList

fetchContents :: (HasScope'
        s
        '["https://www.googleapis.com/auth/drive",
          "https://www.googleapis.com/auth/drive.appdata",
          "https://www.googleapis.com/auth/drive.file",
          "https://www.googleapis.com/auth/drive.metadata",
          "https://www.googleapis.com/auth/drive.metadata.readonly",
          "https://www.googleapis.com/auth/drive.photos.readonly",
          "https://www.googleapis.com/auth/drive.readonly"]
      ~
      'True,
      MonadGoogle s m, MonadThrow m, MonadResource m) => DS.DReq -> m ([DS.DriveFile DS.DriveFileType], BreadCrumbs (DS.DriveFile DS.DriveFileType))
fetchContents (DS.DriveRequest DS.Root _) = do
  fl <- rootContents
  l <- sequence $ fl ^.. flFiles . traverse . to DS.toDriveFile
  return (l, addCrumb DS.Root breadCrumbs)
fetchContents req =
  case df ^? DS.fType of
    Nothing -> throwM $ DS.err400 "Can only display the contents of a folder."
    Just DS.Folder -> do
      let query = df ^? DS.fId . to mkQuery
      fl <- send $ flQ .~ query $ filesList
      l <- sequence $ fl ^.. flFiles . traverse . to DS.toDriveFile
      return (l, addCrumb df $ req ^. reqCrumbs)
    Just _ -> throwM $ DS.err400 "Can only display the contens of a folder."
 where
   df = req ^. reqFile

instance DS.IsDriveFile File where
  toDriveFile f = DS.DriveFile <$> name <*> id <*> typ
    where
      parseType Nothing = throwM $ DS.err400 "No mimetype"
      parseType (Just mt)
        | isZip mt = return DS.Zip
        | isFolder mt = return DS.Folder
        | otherwise = return $ DS.File mt
      g Nothing msg = throwM $ DS.err400 msg
      g (Just v) _ = return v
      name = g (f ^. fName) "The file has no name"
      id = g (f ^. fId) "The file has no id"
      typ = parseType $ f ^. fMimeType

isZip mt = mt `elem` [ "application/x-zip-compressed"
                     , "application/zip"
                     ]

isFolder mt = mt == "application/vnd.google-apps.folder"

serverSettings :: (HasScope'
        s
        '["https://www.googleapis.com/auth/drive",
          "https://www.googleapis.com/auth/drive.appdata",
          "https://www.googleapis.com/auth/drive.file",
          "https://www.googleapis.com/auth/drive.metadata",
          "https://www.googleapis.com/auth/drive.metadata.readonly",
          "https://www.googleapis.com/auth/drive.photos.readonly",
          "https://www.googleapis.com/auth/drive.readonly"]
      ~
      'True,
      HasEnv s r, AllowScopes s) => FilePath -> FilePath -> r -> DS.ServerSettings
serverSettings rootDownloadDir staticDir env = DS.ServerSettings f g staticDir
  where
    f = runResourceT . runGoogle env . fetchContents
    g req = catch (runResourceT . runGoogle env . downloadFile rootDownloadDir $ req) handleErrors
    handleErrors e@(MissingConfigException _) = DS.Handler . DS.throwE $ DS.err400 $ encodeUtf8 $ fromStrict $ tshow e
