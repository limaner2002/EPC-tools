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
import qualified Scheduler as Sched
import Control.Monad.Trans.Except
import Control.Arrow
import Control.Lens
import Data.Time

type HomePageAPI = Get '[HTML] (Html ())
type CreateJobAPI = "new" :> Get '[HTML] (Html ())
type AddJobAPI = "addJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type RemoveJob = "remJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type RunJobsAPI = "runJobs" :> Get '[HTML] (Html ())
type ScheduleJobsAPI = "schedule" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type CancelJobsAPI = "cancel" :> Get '[HTML] (Html ())

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
  :<|> ScheduleJobsAPI
  :<|> CancelJobsAPI

type ServantHandler = (ExceptT ServantErr IO)

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server API
server mkJob jobAct jobsT = homepage jobsT
  :<|> newJob
  :<|> addJob mkJob jobsT
  :<|> runJobs jobAct jobsT
  :<|> scheduleJobs jobAct jobsT
  :<|> cancelJobs jobsT

homepage :: TVar (JobQueue a) -> Server HomePageAPI
homepage jobsT = do
  jobs <- atomically $ readTVar jobsT
  tz <- liftIO getCurrentTimeZone
  return $ renderHomepage tz jobs

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
              _ <- atomically $ do
                modifyTVar jobsT (Sched.addJob $ job jobVal)
                readTVar jobsT
              redirect303 "/"
        where
          -- addIt :: a -> JobQueue a -> JobQueue a
          -- addIt jobVal jobs = jobs <> [job jobVal]
          job jobVal = Job jobName Queued jobVal

application :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Application
application mkJob jobAct jobsT = serve proxyAPI (server mkJob jobAct jobsT)

renderHomepage :: TimeZone -> JobQueue a -> Html ()
renderHomepage tz jobs = 
  case onull (jobs ^. qJobs) of
    True -> do
      header
      p_ "No jobs queued"
      a_ [class_ "pure-button pure-button-primary", href_ "/new"] "Add Job"
      showScheduledTime
      br_ mempty
      br_ mempty
      runJobsButton
    False -> do
      header
      jobTable jobs
      showScheduledTime
      br_ mempty
      br_ mempty
      runJobsButton
    where
      showTime t = formatTime defaultTimeLocale "%F %I:%M %p" $ utcToLocalTime tz t
      showScheduledTime =
        case jobs ^. qStartTime of
          Nothing -> do
            p_ "No start time set"
            br_ mempty
            br_ mempty
            scheduleInput
          Just t -> do
            p_ $ toHtml $ showTime t
            pureButton "/cancel" "Cancel Jobs"


runJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server RunJobsAPI
runJobs f jobsT = do
  _ <- fork $ (Scheduler.runJobs f) jobsT
  redirect303 "/"

scheduleJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server ScheduleJobsAPI
scheduleJobs f jobsT (BodyMap bodyParams) = do
  tz <- liftIO getCurrentTimeZone
  case mTime of
    Nothing -> throwE $ err500 { errBody = "Could not parse the time " }
    Just time -> do
      _ <- atomically $ do
        modifyTVar jobsT (qStartTime .~ Just (localTimeToUTC tz time))
      fork $ Sched.schedule jobsT (Scheduler.runJobs f jobsT)
      redirect303 "/"
  where
    timeTup = (,) <$> lookup "date" bodyParams <*> lookup "time" bodyParams
    timeStr (date, time) = date <> " " <> time <> " "
    mTime :: Maybe LocalTime
    mTime = join $ mapM (parseTimeM True defaultTimeLocale "%F %R %Z") $ fmap (unpack . timeStr) timeTup

cancelJobs :: TVar (JobQueue a) -> Server CancelJobsAPI
cancelJobs jobsT = do
  atomically $ modifyTVar jobsT (qStartTime .~ Nothing)
  redirect303 "/"

redirect303 :: Monad m => ByteString -> ExceptT ServantErr m a
redirect303 url = throwE $ err303 { errHeaders = [("Location", url)] }

schedule :: MonadBase IO m => TVar (JobQueue a) -> m () -> m ()
schedule jobsT action = loop
  where
    loop = do
      ct <- liftBase getCurrentTime
      jobs <- liftBase $ atomically $ readTVar jobsT
      mapM_
        ( \time -> do
            case ct >= time of
              True -> do
                liftBase $ putStrLn "Running job now"
                action
              False -> do
                liftBase $ threadDelay 1000000
                loop
        ) (jobs ^. qStartTime)