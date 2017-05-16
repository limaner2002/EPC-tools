{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scheduler.Server where

import Servant as S
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
import Scheduler.Icons

type HomePageAPI = Get '[HTML] (Html ())
type CreateJobAPI = "new" :> Get '[HTML] (Html ())
type AddJobAPI = "addJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type RemoveJobAPI = "remJob" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type RunJobsAPI = "runJobs" :> Get '[HTML] (Html ())
type ScheduleJobsAPI = "schedule" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type CancelJobsAPI = "cancel" :> Get '[HTML] (Html ())
type MoveJobUpAPI = "moveUp" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type MoveJobDownAPI = "moveDown" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type ArrUpIcon = "icon" :> "arrUp" :> Get '[SvgDiagram] SvgDiagram'
type ArrDownIcon = "icon" :> "arrDown" :> Get '[SvgDiagram] SvgDiagram'
type RemoveIcon = "icon" :> "remove" :> Get '[SvgDiagram] SvgDiagram'

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
  :<|> RemoveJobAPI
  :<|> MoveJobUpAPI
  :<|> MoveJobDownAPI
  :<|> ArrUpIcon
  :<|> ArrDownIcon
  :<|> RemoveIcon

-- type ServantHandler = (ExceptT ServantErr IO)
type ServantHandler = (S.Handler)

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server API
server mkJob jobAct jobsT = homepage jobsT
  :<|> newJob
  :<|> addJob mkJob jobsT
  :<|> runJobs jobAct jobsT
  :<|> scheduleJobs jobAct jobsT
  :<|> cancelJobs jobsT
  :<|> removeJob jobsT
  :<|> moveJobUp jobsT
  :<|> moveJobDown jobsT
  :<|> sendArrUp
  :<|> sendArrDown
  :<|> sendRemove

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
    Nothing -> S.Handler $ throwE $ err500 { errBody = "Could not parse the time " }
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

removeJob :: TVar (JobQueue a) -> Server RemoveJobAPI
removeJob jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.removeJobK idx) jobsT) mIdx
  redirect303 "/"

moveJobUp :: TVar (JobQueue a) -> Server MoveJobUpAPI
moveJobUp jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.moveJobUpK idx) jobsT) mIdx
  redirect303 "/"

moveJobDown :: TVar (JobQueue a) -> Server MoveJobDownAPI
moveJobDown jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.moveJobDownK idx) jobsT) mIdx
  redirect303 "/"

sendArrUp :: Server ArrUpIcon
sendArrUp = return $ SvgDiagram' arrUp

sendArrDown :: Server ArrUpIcon
sendArrDown = return $ SvgDiagram' arrDown

sendRemove :: Server RemoveIcon
sendRemove = return $ SvgDiagram' removeIcon

redirect303 :: ByteString -> S.Handler a
redirect303 url = S.Handler $ throwE $ err303 { errHeaders = [("Location", url)] }

