{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}

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
import Control.Monad.State (MonadState, state, runStateT)

type HomePageAPI = "legacy" :> Get '[HTML] (Html ())
type (AddJobAPI a) = "addJob1" :> ReqBody '[JSON] (Text, Text) :> Post '[JSON] (JobQueue a)
type RemoveJobAPI = "remJob" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type RemoveJobAPI' a = "remJob1" :> QueryParam "idx" Int :> Get '[JSON] (JobQueue a)
type RunJobsAPI a = "runJobs" :> Get '[JSON] (JobQueue a)
type ScheduleJobsAPI = "schedule" :> ReqBody '[BodyParams] BodyMap :> Post '[HTML] (Html ())
type CancelJobsAPI = "cancel" :> Get '[HTML] (Html ())
type MoveJobUpAPI = "moveUp" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type MoveJobDownAPI = "moveDown" :> QueryParam "idx" Int :> Get '[HTML] (Html ())
type ArrUpIcon = "icon" :> "arrUp" :> Get '[SvgDiagram] SvgDiagram'
type ArrDownIcon = "icon" :> "arrDown" :> Get '[SvgDiagram] SvgDiagram'
type RemoveIcon = "icon" :> "remove" :> Get '[SvgDiagram] SvgDiagram'
type GetJobQueueAPI a = "jobQueue" :> Get '[JSON] (JobQueue a)
type KillJobAPI a = "kill" :> Get '[JSON] (JobQueue a)
type ResultsAPI = "results" :> Raw

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
  :<|> RunJobsAPI a
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
  :<|> KillJobAPI a
  :<|> ResultsAPI

type ServantHandler = S.Handler
-- type HandlerT a = S.ServerT a ServantHandler

proxyAPI :: Proxy (API a)
proxyAPI = Proxy

data SchedulerSettings a = SchedulerSettings
  { _schMkJob :: FilePath -> Text -> ServantHandler (Text, a)
  , _schJobAct :: Kleisli ServantHandler a ()
  , _schJobCancel :: TMVar Int
  , _schResultsDir :: FilePath
  , _schJobQueue :: TVar (JobQueue a)
  , _schJobId :: TVar Int
  }

makeLenses ''SchedulerSettings

server :: SchedulerSettings a -> Server (API a)
server schedSettings = homepage jobsT
  :<|> addJob schedSettings
  :<|> runJobs (schedSettings ^. schJobAct) jobsT
  :<|> scheduleJobs (schedSettings ^. schJobAct) jobsT
  :<|> cancelJobs jobsT
  :<|> removeJob jobsT
  :<|> removeJob1 jobsT
  :<|> moveJobUp jobsT
  :<|> moveJobDown jobsT
  :<|> sendArrUp
  :<|> sendArrDown
  :<|> sendRemove
  :<|> sendJobQueue jobsT
  :<|> killJob (schedSettings ^. schJobCancel) jobsT
  :<|> viewResults (schedSettings ^. schResultsDir)
  where
    jobsT = schedSettings ^. schJobQueue

homepage :: TVar (JobQueue a) -> Server HomePageAPI
homepage jobsT = do
  jobs <- atomically $ readTVar jobsT
  tz <- liftIO getCurrentTimeZone
  return $ renderHomepage tz jobs

homeLink :: Link
homeLink = safeLink proxyAPI (Proxy :: Proxy HomePageAPI)

-- addJob :: (FilePath -> Text -> ServantHandler (Text, a)) -> TVar (JobQueue a) -> Server (AddJobAPI a)
addJob :: SchedulerSettings a -> Server (AddJobAPI a)
addJob schedSettings (jobPath, jmxPath) = do
  eJobVal <- tryAny $ (schedSettings ^. schMkJob) (unpack jmxPath) jobPath 
  case eJobVal of
    Left err -> S.Handler $ throwE $ err400 {errBody = encodeUtf8 (fromStrict $ tshow err) }
    Right (jobName, jobVal) ->
      atomically $ do
        jobId <- readTVar jobIdT
        (job, newId) <- runStateT (newJob jobName Queued jobVal) jobId
        writeTVar jobIdT newId
        modifyTVar jobsT (Sched.addJob job)
        readTVar jobsT
          where
            jobsT = schedSettings ^. schJobQueue
            jobIdT = schedSettings ^. schJobId

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

    -- There is clearly a race condition here. Somehow need to 
runJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server (RunJobsAPI a)
runJobs f jobsT = do
  _ <- fork $ (Scheduler.runJobs f) jobsT
  atomically $ readTVar jobsT
  -- redirect303 homeLink

scheduleJobs :: Kleisli ServantHandler a () -> TVar (JobQueue a) -> Server ScheduleJobsAPI
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

cancelJobs :: TVar (JobQueue a) -> Server CancelJobsAPI
cancelJobs jobsT = do
  atomically $ modifyTVar jobsT (qStartTime .~ Nothing)
  redirect303 homeLink

cancelLink :: Link
cancelLink = safeLink proxyAPI (Proxy :: Proxy CancelJobsAPI)

removeJob :: TVar (JobQueue a) -> Server RemoveJobAPI
removeJob jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.removeJobK idx) jobsT) mIdx
  redirect303 homeLink

removeJob1 :: TVar (JobQueue a) -> Server (RemoveJobAPI' a)
removeJob1 jobsT Nothing = S.Handler . throwE $ err400 { errBody = "No id supplied. No job has been removed from the queue." }
removeJob1 jobsT (Just idx) = do
  runKleisli (Scheduler.removeJobK idx) jobsT
  atomically . readTVar $ jobsT

moveJobUp :: TVar (JobQueue a) -> Server MoveJobUpAPI
moveJobUp jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.moveJobUpK idx) jobsT) mIdx
  redirect303 homeLink

moveJobDown :: TVar (JobQueue a) -> Server MoveJobDownAPI
moveJobDown jobsT mIdx = do
  mapM_ (\idx -> runKleisli (Scheduler.moveJobDownK idx) jobsT) mIdx
  redirect303 homeLink

sendArrUp :: Server ArrUpIcon
sendArrUp = return $ SvgDiagram' arrUp

sendArrDown :: Server ArrUpIcon
sendArrDown = return $ SvgDiagram' arrDown

sendRemove :: Server RemoveIcon
sendRemove = return $ SvgDiagram' removeIcon

redirect303 :: Link -> ServantHandler a
redirect303 link = S.Handler $ throwE $ err303 { errHeaders = [("Location", url)] }
  where
    url = encodeUtf8 $ toUrlPiece link

combinedServer :: SchedulerSettings a
  -> ServerSettings -> Server (CombinedAPI a)
combinedServer schedSettings settings =
  server schedSettings :<|> driveServer settings

type CombinedAPI a = API a :<|> DriveAPI

combinedProxy :: Proxy (CombinedAPI a)
combinedProxy = Proxy

sendJobQueue :: TVar (JobQueue a) -> Server (GetJobQueueAPI a)
sendJobQueue = atomically . readTVar

killJob :: TMVar Int -> TVar (JobQueue a) -> Server (KillJobAPI a)
killJob tmVar jobsT = do
  atomically $ do
    empty <- isEmptyTMVar tmVar
    case empty of
      True -> do
        putTMVar tmVar 1
        q <- readTVar jobsT
        case Sched.cancelRunning q of
          Nothing -> readTVar (trace "No jobs running?" jobsT)
          Just (_, q') -> do
            writeTVar jobsT q'
            readTVar jobsT
      False -> readTVar (trace "A job is already in the process of being cancelled." jobsT)

               -- Probably not needed but might be nice for vcache though
newtype SchedT job m a = SchedT { unSched :: ReaderT (SchedulerSettings job) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader (SchedulerSettings job), MonadBase b)

viewResults path = serveDirectory path

-- newtype HandlerT (api :: k) (m :: * -> *) a
--   = HandlerT {runHandler' :: ServerT api m}
--   deriving (Applicative, Functor, Monad)

-- class MonadHandler m a where
--   execHandler :: m a -> IO (Either ServantErr a)

-- instance MonadHandler S.Handler a where
--   execHandler = runHandler

-- addJob' :: (FromJSON a, MonadHandler m a, Monad m) => (Text, Text) -> m (JobQueue a)
-- addJob' _ = return emptyQueue

-- instance MonadState Int m => MonadState Int (HandlerT api m) where
--   state = error "Still under development."

-- addjob' :: (FromJSON a, Monad m) => (Text, Text) -> ServerT (m (JobQueue a)) S.Handler
-- addjob' _ = lift _

-- newtype HandlerState s m a = HandlerState {runHandlerState :: s -> m (S.Handler a, s)}
--   deriving (Functor)

-- instance (Functor m, Monad m) => Applicative (HandlerState s m) where
--   pure a = HandlerState (\s -> return (pure a, s))

--   f <*> g = HandlerState (\s -> do
--                              (hF, s') <- runHandlerState f s
--                              (hA, s'') <- runHandlerState g s'
--                              return (hF <*> hA, s'')
--                          )
-- instance (Monad m, Applicative m) => Monad (HandlerState s m) where
--   x >>= f = HandlerState (\s -> do
--                              (hX, s') <- runHandlerState x s
--                              let h = g s'
--                              _
--                          )
--     where
--       g = flip (runHandlerState . f)
-- -- instance Monad m => MonadState s (HandlerState s m) where
