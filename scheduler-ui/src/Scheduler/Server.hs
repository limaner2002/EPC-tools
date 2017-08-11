
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
import Data.Aeson

type HomePageAPI = "legacy" :> Get '[HTML] (Html ())
type (AddJobAPI a) = "addJob1" :> ReqBody '[JSON] (Text, Text) :> Post '[JSON] (JobQueue a)
type RemoveJobAPI = "remJob" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type RemoveJobAPI' a = "remJob1" :> QueryParam "idx" Int :> Get '[JSON] (JobQueue a)
type RunJobsAPI = "runJobs" :> Get '[HTML] (Html ())
type ScheduleJobsAPI = "schedule" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type CancelJobsAPI = "cancel" :> Get '[HTML] (Html ())
type MoveJobUpAPI = "moveUp" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type MoveJobDownAPI = "moveDown" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type ArrUpIcon = "icon" :> "arrUp" :> Get '[SvgDiagram] SvgDiagram'
type ArrDownIcon = "icon" :> "arrDown" :> Get '[SvgDiagram] SvgDiagram'
type RemoveIcon = "icon" :> "remove" :> Get '[SvgDiagram] SvgDiagram'
type GetJobQueueAPI a = "jobQueue" :> Get '[JSON] (JobQueue a)

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

type API a = HomePageAPI
  :<|> AddJobAPI a
  :<|> RunJobsAPI
  :<|> ScheduleJobsAPI
  :<|> CancelJobsAPI
  :<|> RemoveJobAPI
  :<|> RemoveJobAPI' a
  :<|> MoveJobUpAPI
  :<|> MoveJobDownAPI
  :<|> ArrUpIcon
  :<|> ArrDownIcon
  :<|> RemoveIcon
  :<|> GetJobQueueAPI a

type ServantHandler = S.Handler
type HandlerT a = ServerT a ServantHandler

proxyAPI :: Proxy (API a)
proxyAPI = Proxy

server :: (FilePath -> Text -> ServantHandler (Text, a)) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> HandlerT (API a)
server mkJob jobAct jobsT = homepage jobsT
  :<|> addJob mkJob jobsT
  :<|> runJobs jobAct jobsT
  :<|> scheduleJobs jobAct jobsT
  :<|> cancelJobs jobsT
  :<|> removeJob jobsT
  :<|> removeJob1 jobsT
  :<|> moveJobUp jobsT
  :<|> moveJobDown jobsT
  :<|> sendArrUp
  :<|> sendArrDown
  :<|> sendRemove
  :<|> sendJobQueue jobsT

homepage :: TVar (JobQueue a) -> HandlerT HomePageAPI
homepage jobsT = do
  jobs <- atomically $ readTVar jobsT
  tz <- liftIO getCurrentTimeZone
  return $ renderHomepage tz jobs

homeLink :: Link
homeLink = safeLink proxyAPI (Proxy :: Proxy HomePageAPI)

addJob :: (FilePath -> Text -> ServantHandler (Text, a)) -> TVar (JobQueue a) -> HandlerT (AddJobAPI a)
addJob mkJob jobsT (jobPath, jmxPath) = do
  eJobVal <- tryAny $ mkJob (unpack jmxPath) jobPath 
  case eJobVal of
    Left err -> S.Handler $ throwE $ err400 {errBody = encodeUtf8 (fromStrict $ tshow err) }
    Right (jobName, jobVal) ->
      atomically $ do
        modifyTVar jobsT (Sched.addJob $ job jobName jobVal)
        readTVar jobsT
    where
      job jn jobVal = Job jn Queued jobVal

application :: ToJSON a => (FilePath -> Text -> ServantHandler (Text, a)) -> Kleisli ServantHandler a () -> TVar (JobQueue a) -> Application
application mkJob jobAct jobsT = serve proxyAPI $ server mkJob jobAct jobsT

renderHomepage :: TimeZone -> JobQueue a -> Html ()
renderHomepage tz jobs = 
  case onull (jobs ^. qJobs) of
    True -> do
      header
      p_ "No jobs queued"
      a_ [class_ "pure-button pure-button-primary", href_ $ "badLink"] "Add Job"
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
    Nothing -> S.Handler $ throwE $ err400 { errBody = "Could not parse the time " }
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

removeJob1 :: TVar (JobQueue a) -> HandlerT (RemoveJobAPI' a)
removeJob1 jobsT Nothing = S.Handler . throwE $ err400 { errBody = "No id supplied. No job has been removed from the queue." }
removeJob1 jobsT (Just idx) = do
  runKleisli (Scheduler.removeJobK idx) jobsT
  atomically . readTVar $ jobsT

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

combinedServer :: (FilePath -> Text -> ServantHandler (Text, a)) -> Kleisli ServantHandler a ()
  -> ServerSettings -> TVar (JobQueue a) -> Server (CombinedAPI a)
combinedServer mkJob jobAct settings jobVar =
  server mkJob jobAct jobVar :<|> driveServer settings

type CombinedAPI a = API a :<|> DriveAPI

combinedProxy :: Proxy (CombinedAPI a)
combinedProxy = Proxy

sendJobQueue :: TVar (JobQueue a) -> HandlerT (GetJobQueueAPI a)
sendJobQueue = atomically . readTVar
