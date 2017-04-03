{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scheduler.Server where

import Servant
import Servant.HTML.Lucid
import Lucid
import Scheduler.Lib
import Scheduler.Types
import ClassyPrelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types (parseQueryText)
import qualified Scheduler.Arrow as Scheduler
import Control.Monad.Trans.Except
import Control.Arrow

type HomePageAPI = Get '[HTML] (Html ())
type CreateJobAPI = "new" :> Get '[HTML] (Html ())
type AddJobAPI = "addJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type RemoveJob = "remJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type RunJobsAPI = "runJobs" :> Get '[HTML] (Html ())

data BodyParams
newtype BodyMap = BodyMap (Map Text Text)
  deriving Show

instance Accept BodyParams where
  contentType _ = "application" // "x-www-form-urlencoded"

instance MimeUnrender BodyParams BodyMap where
  mimeUnrender _ bs = BodyMap <$> mkMap
    where
      mkMap = mapFromList <$> paramTuples
      paramTuples = sequence $ fmap mkTuple queryText
      mkTuple (x, Just y) = Right (x, y)
      mkTuple _ = Left "Could not decode body parameters."
      queryText = parseQueryText $ toStrict bs

type API = HomePageAPI
  :<|> CreateJobAPI
  :<|> AddJobAPI
  :<|> RunJobsAPI

type ServantHandler = (ExceptT ServantErr IO)

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server API
server mkJob jobAct jobsT = homepage jobsT
  :<|> newJob
  :<|> addJob mkJob jobsT
  :<|> runJobs jobAct jobsT

homepage :: TVar (JobQueue a) -> Server HomePageAPI
homepage jobsT = do
  jobs <- atomically $ readTVar jobsT
  return $ renderHomepage jobs

newJob :: Server CreateJobAPI
newJob = return $ do
  header
  createJob

addJob :: (Text -> ServantHandler a) -> TVar (JobQueue a) -> Server AddJobAPI
addJob mkJob jobsT (BodyMap bodyParams) = do
  case lookup "job-name" bodyParams of
    Nothing -> pure $ p_ "No job-name supplied!"
    Just jobName -> do
      case lookup "job-val" bodyParams of
        Nothing -> pure $ p_ "No file path supplied!"
        Just jobPath -> do
          eJobVal <- tryAny $ mkJob jobPath
          case eJobVal of
            Left err -> return $ p_ $ toHtml $ tshow err
            Right jobVal -> do
              jobs <- atomically $ do
                modifyTVar jobsT (addIt jobVal)
                readTVar jobsT
              redirect303 "/"
        where
          addIt :: a -> JobQueue a -> JobQueue a
          addIt jobVal jobs = jobs <> [job jobVal]
          job jobVal = Job jobName Queued jobVal

application :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Application
application mkJob jobAct jobsT = serve proxyAPI (server mkJob jobAct jobsT)

renderHomepage :: JobQueue a -> Html ()
renderHomepage jobs =
  case onull jobs of
    True -> do
      header
      p_ "No jobs queued"
      a_ [class_ "pure-button pure-button-primary", href_ "/new"] "Add Job"
      runJobsButton
    False -> do
      header
      jobTable jobs
      br_ mempty
      runJobsButton

runJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server RunJobsAPI
runJobs f jobsT = do
  _ <- fork $ (Scheduler.runJobs f) jobsT
  redirect303 "/"

redirect303 :: Monad m => ByteString -> ExceptT ServantErr m a
redirect303 url = throwE $ err303 { errHeaders = [("Location", url)] }
