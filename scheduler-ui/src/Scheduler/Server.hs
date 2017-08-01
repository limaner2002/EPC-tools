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
import Scheduler.Google.Server (driveServer, DriveAPI, ServerSettings)

type HomePageAPI = "legacy" :> Get '[HTML] (Html ())
type CreateJobAPI = "new" :> Get '[HTML] (Html ())
type AddJobAPI = "addJob" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type AddJobAPI' = "addJob1" :> ReqBody '[JSON] Text :> Post '[HTML] (Html ())
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
  :<|> AddJobAPI'
  :<|> RunJobsAPI
  :<|> ScheduleJobsAPI
  :<|> CancelJobsAPI
  :<|> RemoveJobAPI
  :<|> MoveJobUpAPI
  :<|> MoveJobDownAPI
  :<|> ArrUpIcon
  :<|> ArrDownIcon
  :<|> RemoveIcon

type ServantHandler = S.Handler
type HandlerT a = ServerT a ServantHandler

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: (Text -> ServantHandler (Text, a)) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> HandlerT API
server mkJob jobAct jobsT = homepage jobsT
  :<|> newJob
  :<|> addJob mkJob jobsT
  :<|> addJob' mkJob jobsT
  :<|> runJobs jobAct jobsT
  :<|> scheduleJobs jobAct jobsT
  :<|> cancelJobs jobsT
  :<|> removeJob jobsT
  :<|> moveJobUp jobsT
  :<|> moveJobDown jobsT
  :<|> sendArrUp
  :<|> sendArrDown
  :<|> sendRemove

homepage :: TVar (JobQueue a) -> HandlerT HomePageAPI
homepage jobsT = do
  jobs <- atomically $ readTVar jobsT
  tz <- liftIO getCurrentTimeZone
  return $ renderHomepage tz jobs

homeLink :: Link
homeLink = safeLink proxyAPI (Proxy :: Proxy HomePageAPI)

newJob :: HandlerT CreateJobAPI
newJob = return $ do
  header
  createJob

createJobLink :: Link
createJobLink = safeLink proxyAPI (Proxy :: Proxy CreateJobAPI)

addJob :: (Text -> ServantHandler (Text, a)) -> TVar (JobQueue a) -> HandlerT AddJobAPI
addJob mkJob jobsT (BodyMap bodyParams) = do
  case lookup "job-val" bodyParams of
    Nothing -> pure $ p_ "No file path supplied!"
    Just jobPath -> do
      eJobVal <- tryAny $ mkJob jobPath
      case eJobVal of
        Left err -> return $ p_ $ toHtml $ tshow err
        Right (jobName, jobVal) -> do
          _ <- atomically $ do
            modifyTVar jobsT (Sched.addJob $ job jobName jobVal)
            readTVar jobsT
          redirect303 $ homeLink
    where
      job jn jobVal = Job jn Queued jobVal

addJob' :: (Text -> ServantHandler (Text, a)) -> TVar (JobQueue a) -> HandlerT AddJobAPI'
addJob' mkJob jobsT jobPath = do
  eJobVal <- tryAny $ mkJob jobPath
  case eJobVal of
    Left err -> return $ p_ $ toHtml $ tshow err
    Right (jobName, jobVal) -> do
          _ <- atomically $ do
            modifyTVar jobsT (Sched.addJob $ job jobName jobVal)
            readTVar jobsT
          redirect303 $ homeLink
    where
      job jn jobVal = Job jn Queued jobVal

application :: (Text -> ServantHandler (Text, a)) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Application
application mkJob jobAct jobsT = serve proxyAPI $ server mkJob jobAct jobsT

renderHomepage :: TimeZone -> JobQueue a -> Html ()
renderHomepage tz jobs = 
  case onull (jobs ^. qJobs) of
    True -> do
      header
      p_ "No jobs queued"
      a_ [class_ "pure-button pure-button-primary", href_ $ toUrlPiece createJobLink] "Add Job"
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
            pureButton (toUrlPiece cancelLink) "Cancel Jobs"

runJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> HandlerT RunJobsAPI
runJobs f jobsT = do
  _ <- fork $ (Scheduler.runJobs f) jobsT
  redirect303 homeLink

scheduleJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> HandlerT ScheduleJobsAPI
scheduleJobs f jobsT (BodyMap bodyParams) = do
  tz <- liftIO getCurrentTimeZone
  case mTime of
    Nothing -> S.Handler $ throwE $ err500 { errBody = "Could not parse the time " }
    Just time -> do
      _ <- atomically $ do
        modifyTVar jobsT (qStartTime .~ Just (localTimeToUTC tz time))
      fork $ Sched.schedule jobsT (Scheduler.runJobs f jobsT)
      redirect303 homeLink
  where
    timeTup = (,) <$> lookup "date" bodyParams <*> lookup "time" bodyParams
    timeStr (date, time) = date <> " " <> time <> " "
    mTime :: Maybe LocalTime
    mTime = join $ mapM (parseTimeM True defaultTimeLocale "%F %R %Z") $ fmap (unpack . timeStr) timeTup

cancelJobs :: TVar (JobQueue a) -> HandlerT CancelJobsAPI
cancelJobs jobsT = do
  atomically $ modifyTVar jobsT (qStartTime .~ Nothing)
  redirect303 homeLink

cancelLink :: Link
cancelLink = safeLink proxyAPI (Proxy :: Proxy CancelJobsAPI)

removeJob :: TVar (JobQueue a) -> HandlerT RemoveJobAPI
removeJob jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.removeJobK idx) jobsT) mIdx
  redirect303 homeLink

moveJobUp :: TVar (JobQueue a) -> HandlerT MoveJobUpAPI
moveJobUp jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.moveJobUpK idx) jobsT) mIdx
  redirect303 homeLink

moveJobDown :: TVar (JobQueue a) -> HandlerT MoveJobDownAPI
moveJobDown jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.moveJobDownK idx) jobsT) mIdx
  redirect303 homeLink

sendArrUp :: HandlerT ArrUpIcon
sendArrUp = return $ SvgDiagram' arrUp

sendArrDown :: HandlerT ArrUpIcon
sendArrDown = return $ SvgDiagram' arrDown

sendRemove :: HandlerT RemoveIcon
sendRemove = return $ SvgDiagram' removeIcon

redirect303 :: Link -> ServantHandler a
redirect303 link = S.Handler $ throwE $ err303 { errHeaders = [("Location", url)] }
  where
    url = encodeUtf8 $ toUrlPiece link

combinedServer :: (Text -> ServantHandler (Text, a)) -> Kleisli ServantHandler a ()
  -> ServerSettings -> TVar (JobQueue a) -> Server CombinedAPI
combinedServer mkJob jobAct settings jobVar =
  server mkJob jobAct jobVar :<|> driveServer settings

type CombinedAPI = API :<|> DriveAPI

combinedProxy :: Proxy CombinedAPI
combinedProxy = Proxy
