{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ClassyPrelude
import Servant
import Servant.Client hiding (responseStatus)
import Network.HTTP.Client.TLS
import Network.HTTP.Client ( newManager, defaultManagerSettings, managerModifyRequest
                           , managerModifyResponse, responseStatus, responseHeaders
                           , responseCookieJar, CookieJar, destroyCookieJar, cookie_name
                           , cookie_value
                           )
import Servant.Common.Req (Req (..), performRequest, performRequestNoBody)
import Control.Lens
import qualified Web.Cookie as WC
import Network.HTTP.Media ((//), (/:))
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens

type TestAPI = Get '[HTML] CookieJar
type LoginAPI = "suite" :> "auth" :> QueryParam "appian_environment" Text :> Login :> LoginCj :> Post '[HTML] CookieJar
type LogoutAPI = "suite" :> "logout" :> CookieJar :> Get '[HTML] ByteString
type RecordsTab = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "view" :> "all" :> CookieJar :> Get '[AppianApplication] Value
type ViewRecord = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "type" :> RecordId :> "view" :> "all" :> CookieJar :> Get '[AppianApplication] Value
type ViewRecordEntry = "suite" :> "rest" :> "a" :> "record" :> "latest" :> PathPiece RecordRef :> "dashboards" :> "summary" :> CookieJar :> Get '[InlineSail] Value
type TasksTab = "suite" :> "api" :> "feed" :> TaskParams :> QueryParam "b" UTCTime :> AppianCsrfToken :> Get '[JSON] Value
type AcceptTask = "suite" :> "rest" :> "a" :> "task" :> "latest" :> PathPiece TaskId :> "accept" :> AppianCsrfToken :> Post '[JSON] NoContent
type TaskStatus = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> PathPiece TaskId :> "status" :> ReqBody '[PlainText] Text :> AppianCsrfToken :> Put '[AppianTVUI] Value
type TaskAttributes = "suite" :> "rest" :> "a" :> "task" :> "latest" :> PathPiece TaskId :> "attributes" :> AppianCsrfToken :> Get '[JSON] Value
type TaskUpdate update = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Header "X-Appian-Features" Text :> Header "X-Client-Mode" Text :> Header "X-Appian-Ui-State" Text :> Header "Accept-Language" Text
  :> PathPiece TaskId :> "form" :> ReqBody '[AppianTV] (UiConfig update) :> AppianCsrfToken :> Post '[AppianTVUI] Value
type ReportsTab = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> "reports" :> AppianCsrfToken :> Get '[AtomApplication, JSON] Value
type EditReport = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> PathPiece ReportId :> "view" :> AppianCsrfToken :> Get '[AppianTVUI] Value
type UIUpdate update = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> PathPiece ReportId :> "view" :> ReqBody '[AppianTV] (UiConfig update) :> AppianCsrfToken :> Post '[AppianTVUI] Value

test3ClientEnv = do
  mgr <- newManager tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Https "portal-test3.appiancloud.com" 443 "")

test3ClientEnvDbg = do
  mgr <- newManager settings
  return $ ClientEnv mgr (BaseUrl Https "portal-test3.appiancloud.com" 443 "")
    where
      reqLoggerFunc req = print req >> return req
      settings = tlsManagerSettings { managerModifyRequest = reqLoggerFunc
                                    , managerModifyResponse = respLoggerFunc
                                    }
      respLoggerFunc resp = do
        print (responseStatus resp)
        print (responseHeaders resp)
        print (responseCookieJar resp)
        return resp

type API = TestAPI
  :<|> LoginAPI
  :<|> LogoutAPI
  :<|> RecordsTab
  :<|> ViewRecord
  :<|> ViewRecordEntry
  :<|> TasksTab
  :<|> TaskAttributes
  :<|> AcceptTask
  :<|> TaskStatus
  :<|> ReportsTab
  :<|> EditReport
--  :<|> TaskUpdate update

api :: Proxy API
api = Proxy

navigateSite
  :<|> login
  :<|> logout
  :<|> recordsTab_
  :<|> viewRecord_
  :<|> viewRecordEntry_
  :<|> tasksTab_
  :<|> taskAttributes_
  :<|> acceptTask_
  :<|> taskStatus_
  :<|> reportsTab_
  :<|> editReport_
  = client api

recordsTab = Appian recordsTab_
viewRecord r = Appian (viewRecord_ r)
viewRecordEntry e = Appian (viewRecordEntry_ e)
tasksTab mUtcTime = Appian (tasksTab_ mUtcTime)
taskAttributes tid = Appian (taskAttributes_ tid)
acceptTask tid = Appian (acceptTask_ tid)
taskStatus tid = Appian (taskStatus_ (Just ("ceebc" :: Text)) (Just ("stateful" :: Text)) tid ("accepted" :: Text))
reportsTab = Appian reportsTab_
editReport rid = Appian (editReport_ rid)

taskUpdate :: ToJSON update => PathPiece TaskId -> UiConfig update -> Appian Value
taskUpdate tid ui = Appian (taskUpdate_ tid ui)
  where
    taskUpdate_ = client (Proxy :: ToJSON update => Proxy (TaskUpdate update)) (Just ("ceebc" :: Text)) (Just ("TEMPO" :: Text)) (Just ("stateful" :: Text)) (Just ("en-US,en;q=0.8" :: Text))

uiUpdate :: ToJSON update => PathPiece ReportId -> UiConfig update -> Appian Value
uiUpdate rid ui = Appian (uiUpdate_ rid ui)
  where
    uiUpdate_ = client (Proxy :: ToJSON update => Proxy (UIUpdate update))

bracket' :: MonadCatch m => m a -> (a -> m b) -> (a -> m c) -> m (Either SomeException b)
bracket' init f fin = do
      v <- init
      res <- tryAny (f v)
      fin v
      return res

localClientEnv = do
  mgr <- newManager defaultManagerSettings
  return $ ClientEnv mgr (BaseUrl Http "localhost" 3000 "")  

runAppian :: Appian a -> ClientEnv -> Login -> IO (Either ServantError a)
runAppian f env creds = bracket (runClientM login' env) (\cj -> runClientM (logout' cj) env) runF
  where
    login' = do
      cj <- navigateSite
      login (Just ("tempo" :: Text)) creds $ LoginCj cj
    logout' (Left exc) = return $ Left exc
    logout' (Right session) = do
      _ <- logout session
      return $ Right ()
    runF (Left exc) = return $ Left exc
    runF (Right session) = runClientM (unAppian f session) env

testCreds = Login "app.full.right@testmail.usac.org" "Usac123$"

initialReviewer = Login "initial.reviewer@testmail.usac.org" "USACuser123!"

test3Admin = Login "EPC.Application.Administrator" "USACuser123!"

view486 :: IO ()
view486 = do
  env <- test3ClientEnv
  res <- runAppian (do
                v <- recordsTab
                let rids = v ^.. deep (filtered $ has $ key "label" . _String . only "FCC Forms 486") . key "value" . key "urlstub" . _String . to (RecordId . unpack)
                return rids
            ) env testCreds
  print res

viewAttrs :: Login -> IO ()
viewAttrs creds = do
  env <- test3ClientEnv
  res <- runAppian (do
                v <- tasksTab Nothing
                let tids = v ^.. deep (key "entries") . _Array . traverse . key "id" . _String . to (stripPrefix "t-") . _Just . to (PathPiece . TaskId)
                attrs <- mapM taskAttributes tids
                mapM_ print attrs

                res <- mapM acceptTask tids
                mapM_ print res

                res' <- mapM taskStatus tids
                mapM_ print res'
            ) env creds
  print res

suffixed
  :: (Eq (Element a), IsSequence a, Choice p, Applicative f) =>
     a -> p () (f ()) -> p a (f a)
suffixed a = prism' (\() -> a) $ guard . (isSuffixOf a)

prefixed
  :: (Eq (Element a), IsSequence a, Choice p, Applicative f) =>
     a -> p () (f ()) -> p a (f a)
prefixed a = prism' (\() -> a) $ guard . (isPrefixOf a)

getCurrentTaskIds :: Appian [Text]
getCurrentTaskIds = do
  v <- tasksTab Nothing
  return $ v ^.. getTaskIds

getTaskIds :: (Contravariant f, AsValue s, Plated s, Applicative f) => (Text -> f Text) -> s -> f s
getTaskIds = deep (key "entries") . _Array . traverse . key "id" . _String . to (stripPrefix "t-") . _Just

getCloseButton :: (Contravariant f, AsJSON s, AsValue s, Plated s, Applicative f) => (ButtonWidget -> f ButtonWidget) -> s -> f s
getCloseButton = deep (filtered $ has $ key "label" . _String . filtered (\label -> label == "Close" || label == "Cancel" || label == "Reject")) . _JSON

getEditNotesIds :: Maybe UTCTime -> Appian ([(Text, Text)], Maybe UTCTime)
getEditNotesIds mTime = do
  v <- tasksTab mTime
  --let next = v ^? deep (filtered $ has $ key "rel" . _String . only "next") . key "href" . _String . to (lookup "b" . toQueryMap . parseQueryHack) . traverse . to readMay . traverse
  let next = v ^? deep (filtered $ has $ key "rel" . _String . only "next") . key "href" . _String . to (lookup "b" . toQueryMap . parseQueryHack) . traverse . to tshow . _JSON
  return $ (v ^.. deep (filtered $ has $ key "content") . runFold ((,) <$> Fold (key "content" . _String) <*> Fold (key "id" . _String)), next)

getReportLink :: Text -> Value -> Maybe Text
getReportLink label v = v ^? deep (filtered $ has $ key "title" . _String . only "My Assigned 471 Applications") . key "links" . _Array . traverse . filtered (has $ key "rel" . _String . only "edit") . key "href" . _String

data NoCancelException = NoCancelException

instance Exception NoCancelException

instance Show NoCancelException where
  show _ = "Could not locate the 'cancel' button"

cancelTask :: TaskId -> Appian Value
cancelTask tid = do
  v <- taskStatus $ PathPiece tid
  let mReq = (_Just . uiUpdates .~ (SaveRequestList . pure . toUpdate <$> v ^? getCloseButton)) $ v ^? _JSON . to asUiConfig
      asUiConfig = id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)
  case mReq of
    Nothing -> trace "Throwing an error right now!" (throwM NoCancelException)
    Just req -> taskUpdate (PathPiece tid) req

cancelTasks :: [(Text, TaskId)] -> Appian [Value]
cancelTasks = mapM cancelTask'
  where
    cancelTask' (label, tid) = do
      putStrLn $ "Cancelling task: " <> tshow label
      res <- tryAny $ cancelTask tid
      case res of
        Left err -> do
          print err
          return Null
        Right v -> return v

cancelThem :: Appian ()
cancelThem = go Nothing
  where
    go mTime = do
      (tids, mNext) <- getEditNotesIds mTime
      let tids' = tids ^.. traverse . runFold ((,) <$> Fold _1 <*> Fold (_2 . to (stripPrefix "t-") . _Just . to TaskId))
      _ <- cancelTasks tids'
      putStrLn "Done!"
      case mNext of
        Nothing -> do
          putStrLn "No more tasks to cancel!"
          return ()
        Just time -> go $ Just time

assignedPostCommit :: Appian Value
assignedPostCommit =
      editReport rid
  >>= sendSelection rid "Review Type" 2
  >>= sendSelection rid "Reviewer Type" 2
  >>= sendSelection rid "Funding Year" 2
  >>= clickButton rid "Apply Filters"
  where
    rid = PathPiece $ ReportId "yMOz4g"

newtype MissingComponentException = MissingComponentException Text

instance Show MissingComponentException where
  show (MissingComponentException name) = "MissingComponentException: Could not find component " <> show name

instance Exception MissingComponentException

sendSelection :: PathPiece ReportId -> Text -> Int -> Value -> Appian Value
sendSelection rid label n v = handleMissing label =<< (sequence $ uiUpdate rid <$> up)
  where
    up = join $ mkUpdate <$> (_Just . dfValue .~ n $ dropdown) <*> pure v
    dropdown = v ^? getDropdown label

clickButton :: PathPiece ReportId -> Text -> Value -> Appian Value
clickButton rid label v = handleMissing label =<< (sequence $ uiUpdate rid <$> up)
  where
    up = join $ mkUpdate <$> btn <*> pure v
    btn = v ^? getButton label

handleMissing :: Text -> Maybe Value -> Appian Value
handleMissing label Nothing = throwM $ MissingComponentException label
handleMissing label (Just component) = return component

makeSelection :: Value -> Text -> Int -> Maybe DropdownField
makeSelection v label n = v ^? hasKeyValue "label" label . _JSON . to (dfValue .~ n)

mkUpdate :: ToUpdate a => a -> Value -> Maybe (UiConfig (SaveRequestList Update))
mkUpdate component v = v ^? _JSON . to (id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)) . to (uiUpdates .~ (Just . SaveRequestList . pure . toUpdate $ component))

parseQueryHack :: Text -> [[[Text]]]
parseQueryHack = (fmap . fmap) (splitSeq "=") . fmap (splitSeq "&") . drop 1 . splitElem '?'

toQueryMap :: [[[Text]]] -> Map Text Text
toQueryMap l = mapFromList $ l ^.. traverse . traverse . filtered (\l' -> length l' == 2) . to mkPair
  where
    mkPair [x, y] = (x, y)

main :: IO ()
main = do
  env <- test3ClientEnv
  res <- runAppian cancelThem env initialReviewer
  print res
