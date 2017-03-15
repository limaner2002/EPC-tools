{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server where

import Servant
import Servant.HTML.Lucid
import Lucid
import Lib
import Types
import ClassyPrelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types (parseQueryText)

type HomePageAPI = Get '[HTML] (Html ())
type CreateJobAPI = "new" :> Get '[HTML] (Html ())
type AddJobAPI = "addJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type RemoveJob = "remJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())

data BodyParams
newtype BodyMap = BodyMap (Map Text Text)
  deriving Show

instance Accept BodyParams where
  contentType _ = "application" // "x-www-form-urlencoded" -- "text" // "plaintext" /: ("charset", "utf-8")

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

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: TVar JobQueue -> Server API
server jobsT = homepage jobsT
  :<|> newJob
  :<|> addJob jobsT

homepage :: TVar JobQueue -> Server HomePageAPI
homepage jobsT = do
  jobs <- atomically $ readTVar jobsT
  case onull jobs of
    True -> pure $ do
      header
      p_ "No jobs queued"
      a_ [class_ "pure-button pure-button-primary", href_ "/new"] "Add Job"
    False -> return $ do
      header
      jobTable $ fmap snd $ mapToList jobs -- [Job "6742" Queued, Job "4412" Running, Job "6781" Finished]

newJob :: Server CreateJobAPI
newJob = return $ do
  header
  createJob

addJob :: TVar JobQueue -> Server AddJobAPI
addJob jobsT (BodyMap bodyParams) = do
  case lookup "job-name" bodyParams of
    Nothing -> pure $ p_ "No job-name supplied!"
    Just jobName -> do
      atomically $ modifyTVar jobsT addIt
      pure $ p_ "Successfully added the job!"
     where
       addIt :: JobQueue -> JobQueue
       addIt jobs = insertMap jobName job jobs
       job = Job jobName Queued

application :: TVar JobQueue -> Application
application jobsT = serve proxyAPI (server jobsT)
