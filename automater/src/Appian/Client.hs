{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Appian.Client
  ( module Appian.Client
  , Servant.Client.Core.RunClient
  , Control.Monad.Logger.MonadLogger
  , Servant.Client.ServantError
  ) where

import Servant.API
import Servant.Client hiding (responseStatus, HasClient, Client)
import Servant.Client.Core hiding (ServantError, BaseUrl, Https)
import Control.Lens hiding (cons, index)
import Control.Lens.Action.Reified
import Control.Lens.Action
import ClassyPrelude
import Data.Proxy
import qualified Web.Cookie as WC
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS
import Control.Monad.Logger
import Control.Monad.State (put, get)
import Data.Aeson
import Appian.Instances
import Appian.Types
import Appian
import Appian.Lens
import Data.Aeson.Lens
import Data.Time (diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Monad.Time
import Util.Parallel (runParallelFileLoggingT)
import Data.Random
import Control.Monad.Except
import Network.HTTP.Types.Status (statusCode, statusMessage, Status, status200, status400)
import qualified Data.Csv as Csv

type HomepageAPI = Get '[HTML] (Headers '[Header "Set-Cookie" Text] NoContent)
type LoginAPI = "suite" :> "auth" :> QueryParam "appian_environment" Text :> QueryParam "un" Text :> QueryParam "pw" Text :> QueryParam "X-APPIAN-CSRF-TOKEN" Text
  :> Header "User-Agent" UserAgent :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] ByteString)

type RecordsTab = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "view" :> "all" :> Header "User-Agent" UserAgent :> Get '[AppianApplication] Value

type LogoutAPI = "suite" :> "logout" :> Get '[HTML] ByteString

type ReportsTab = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> "reports" :> Get '[AtomApplication, JSON] Value

type TasksTab = "suite" :> "api" :> "feed" :> "tempo" :> QueryParam "m" Text :> QueryParam "t" Text :> QueryParam "s" Text
                :> QueryParam "defaultFacets" Text :> QueryParam "b" UTCTime :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] Value

type ViewRecord = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "type" :> Capture "recordId" RecordId :> "view" :> "all"
  :> Header "X-Appian-Features" Text :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[AppianTVUI] Value

type RecordUpdate update = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> TrailingSlash :> ReqBody '[AppianTV] (UiConfig update)
  :> Header "X-Appian-Features" Text :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type ViewRecordDashboard = "suite" :> "rest" :> "a" :> "record" :> "latest" :> Capture "recordRef" RecordRef :> "dashboards" :> Capture "dashboard" Dashboard :> Get '[InlineSail] Value

type EditReport = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> Capture "repordId" ReportId :> "view" :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[AppianTVUI] Value

type ReportUpdate update = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text
  :> Capture "recordId" ReportId :> "view" :> ReqBody '[AppianTV] (UiConfig update) :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type TaskUpdate update = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Header "X-Appian-Features" Text :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text
  :> Capture "taskId" TaskId :> "form" :> ReqBody '[AppianTV] (UiConfig update) :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type TaskAccept = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Capture "taskId" TaskId :> "accept" :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[JSON] NoContent
type TaskOpen = "suite" :> "api" :> "feed" :> "tempo" :> Capture "t-taskId" TTaskId :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] Value
type TaskStatus = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Capture "taskId" TaskId :> "status" :> ReqBody '[PlainText] Text :>
  Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Header "x-http-method-override" Text :> Post '[AppianTVUI] Value

type LandingPageAction = "suite" :> "api" :> "tempo" :> "open-a-case" :> "action" :> Capture "actionId" ActionId :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] ProcessModelId

type LandingPageActionEx = "suite" :> "rest" :> "a" :> "model" :> "latest" :> Capture "processModelId" ProcessModelId :> ReqBody '[EmptyAppianTV] EmptyAppianTV :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type RelatedActionEx = "suite" :> "rest" :> "a" :> "record" :> "latest" :> Capture "recordRef" RecordRef :> "action" :> Capture "actionId" ActionId :> ReqBody '[EmptyAppianTV] EmptyAppianTV
   :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type ActionsTab = "suite" :> "api" :> "tempo" :> "open-a-case" :> "available-actions" :> Header "X-Appian-Features" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] Value
type ActionEx = "suite" :> "rest" :> "a" :> "model" :> "latest" :> Capture "processModelId" ProcessModelId
  :> ReqBody '[EmptyAppianTV] EmptyAppianTV :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

getCookies :: (Applicative f, GetHeaders ls, Contravariant f) =>
  ((ByteString, ByteString) -> f (ByteString, ByteString)) -> ls -> f ls
getCookies = to getHeaders . traverse . traverse . to (splitSeq "\n") . traverse . to WC.parseCookies . to (take 1) . traverse

toClient :: HasClient m api => Proxy m -> Proxy api -> Client m api
toClient proxy = flip clientIn proxy

navigateSite :: RunClient m => m (Headers '[Header "Set-Cookie" Text] NoContent)
navigateSite = toClient Proxy (Proxy :: Proxy HomepageAPI)

getCSRF :: (Contravariant f, Choice p, Applicative f) =>
           p (ByteString, ByteString) (f (ByteString, ByteString))
        -> p (ByteString, ByteString) (f (ByteString, ByteString))
getCSRF = filtered (has $ _1 . only "__appianCsrfToken")

remCSRF :: (Contravariant f, Choice p, Applicative f) =>
           p (ByteString, ByteString) (f (ByteString, ByteString))
        -> p (ByteString, ByteString) (f (ByteString, ByteString))
remCSRF = filtered (hasn't $ _1 . only "__appianCsrfToken")

login :: RapidFire m => Login -> AppianT m (Headers '[Header "Set-Cookie" Text] ByteString)
login (Login un pw) = do
  bounds <- use appianBounds
  res <- thinkTimer bounds $ recordTime "Navigate to site" navigateSite
  -- res <- navigateSite
  let cj = res ^.. getCookies & Cookies
  assign appianCookies cj
  res' <- thinkTimer bounds $ recordTime "Login" $ login_ (Just "TEMPO") (Just un) (Just pw) (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8) (Just defUserAgent)
  -- res' <- login_ (Just "TEMPO") (Just un) (Just pw) (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8) (Just defUserAgent)
  assign appianCookies $ Cookies $ (cj ^.. unCookies . traverse . filtered removeNew) <> res' ^.. getCookies
  return res'
  where
    login_ = toClient Proxy (Proxy :: Proxy LoginAPI)
    removeNew (n, _) = not (n == "JSESSIONID" || n == "__appianCsrfToken")

recordsTab :: RapidFire m => AppianT m Value
recordsTab = do
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "Opening Records Tab" $ recordsTab_ (Just defUserAgent)
  where
    recordsTab_ = toClient Proxy (Proxy :: Proxy RecordsTab)

-- recordsTab' :: RapidFire m => AppianT m ()
-- recordsTab' = do
--   v <- recordsTab
--   assign appianValue v

reportsTab :: RapidFire m => AppianT m Value
reportsTab = do
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "Opening Reports Tab" $ toClient Proxy (Proxy :: Proxy ReportsTab)

logout :: RapidFire m => AppianT m ByteString
logout = do
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "Logout" $ toClient Proxy (Proxy :: Proxy LogoutAPI)

tasksTab :: RapidFire m => Maybe UTCTime -> AppianT m Value
tasksTab mUtcTime = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "Opening Tasks Tab" $ tasksTab_ (Just "menu-tasks") (Just "t") (Just "pt") (Just "%5Bstatus-open%5D") mUtcTime (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    tasksTab_ = toClient Proxy (Proxy :: Proxy TasksTab)

viewRecord :: RapidFire m => RecordId -> AppianT m Value
viewRecord rid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "View Record" $ viewRecord_ rid (Just ("ceebc" :: Text)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    viewRecord_ = toClient Proxy (Proxy :: Proxy ViewRecord)

recordUpdate :: (RunClient m, ToJSON update) => UiConfig update -> AppianT m Value
recordUpdate upd = do
  cj <- use appianCookies
  recordUpdate_ upd (Just ("ceebc" :: Text)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    recordUpdate_ = toClient Proxy (Proxy :: Proxy (RecordUpdate update))

sendRecordUpdates :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
sendRecordUpdates msg fold = do
  previousVal <- use appianValue
  eVal <- sendUpdates_ recordUpdate msg fold previousVal
  case eVal of
    Left v -> throwError v
    Right newVal -> do
      res <- deltaUpdate previousVal newVal
      assign appianValue res

editReport :: RapidFire m => ReportId -> AppianT m Value
editReport rid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("Edit Report " <> tshow rid) $ toClient Proxy (Proxy :: Proxy EditReport) rid (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)

reportUpdate :: (RunClient m, ToJSON update) => ReportId -> UiConfig update -> AppianT m Value
reportUpdate rid upd = do
  cj <- use appianCookies
  reportUpdate_ (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO") rid upd (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      reportUpdate_ = toClient Proxy (Proxy :: ToJSON update => Proxy (ReportUpdate update))

taskUpdate :: (RunClient m, ToJSON update) => TaskId -> UiConfig update -> AppianT m Value
taskUpdate tid upd = do
  cj <- use appianCookies
  taskUpdate_ (Just ("ceebc" :: Text)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO") tid upd (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    taskUpdate_ = toClient Proxy (Proxy :: ToJSON update => Proxy (TaskUpdate update))

taskAccept :: RapidFire m => TaskId -> AppianT m NoContent
taskAccept tid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("Accept Task: "<> tshow tid) $ taskAccept_ tid (Just "e4bc") (Just "stateful") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    taskAccept_ = toClient Proxy (Proxy :: Proxy TaskAccept)

taskOpen :: RapidFire m => TTaskId -> AppianT m Value
taskOpen tid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("Open task: " <> tshow tid) $ taskOpen_ tid (Just "e4bc") (Just "stateful") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    taskOpen_ = toClient Proxy (Proxy :: Proxy TaskOpen)

taskStatus :: RapidFire m => TaskId -> AppianT m Value
taskStatus tid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("Task Status: " <> tshow tid) $ taskStatus_ tid "accepted" (Just "ceebc") (Just "stateful") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8) (Just "PUT")
  where
    taskStatus_ = toClient Proxy (Proxy :: Proxy TaskStatus)

landingPageAction :: RapidFire m => ActionId -> AppianT m ProcessModelId
landingPageAction aid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("Launch Action: " <> tshow aid) $ landingPageAction_ aid (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      landingPageAction_ = toClient Proxy (Proxy :: Proxy LandingPageAction)

landingPageActionEx :: RunClient m => ProcessModelId -> AppianT m Value
landingPageActionEx pid = do
  cj <- use appianCookies 
  landingPageActionEx_ pid EmptyAppianTV (Just "ceebc") (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      landingPageActionEx_ = toClient Proxy (Proxy :: Proxy LandingPageActionEx)

viewRecordDashboard :: RapidFire m => RecordRef -> Dashboard -> AppianT m Value
viewRecordDashboard rref dashboard = do
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("View Record Dashboard: " <> tshow dashboard) $ viewRecordDashboard_ rref dashboard
    where
      viewRecordDashboard_ = toClient Proxy (Proxy :: Proxy ViewRecordDashboard)

relatedActionEx :: RapidFire m => RecordRef -> ActionId -> AppianT m Value
relatedActionEx ref aid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "View Related Actions" $ relatedActionEx_ ref aid EmptyAppianTV (Just "ceebc") (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      relatedActionEx_ = toClient Proxy (Proxy :: Proxy RelatedActionEx)

actionsTab :: RapidFire m => AppianT m Value
actionsTab = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime "Opening Actions Tab" $ actionsTab_ (Just "e4bc") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      actionsTab_ = toClient Proxy (Proxy :: Proxy ActionsTab)

    -- Environments

actionEx :: RapidFire m => ProcessModelId -> AppianT m Value
actionEx pid = do
  cj <- use appianCookies
  bounds <- use appianBounds
  thinkTimer bounds $ recordTime ("Execute Action: " <> tshow pid) $ actionEx_ pid EmptyAppianTV (Just "ceebc") (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      actionEx_ = toClient Proxy (Proxy :: Proxy ActionEx)

newtype HostUrl = HostUrl String
  deriving (Show, Eq, IsString)

clientEnvDbgWith :: (C.Request -> IO C.Request) -> HostUrl -> IO ClientEnv
clientEnvDbgWith f (HostUrl url) = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https "portal-test3.appiancloud.com" 443 "")
    where
      reqLoggerFunc req = f req
      settings = TLS.tlsManagerSettings { C.managerModifyRequest = reqLoggerFunc
                                        , C.managerModifyResponse = respLoggerFunc
                                        }
      respLoggerFunc resp = do
        print (C.responseStatus resp)
        print (C.responseHeaders resp)
        print (C.responseCookieJar resp)
        cookieModifier resp

clientEnvDbg :: HostUrl -> IO ClientEnv
clientEnvDbg = clientEnvDbgWith (\req -> print req >> return req)

cookieModifier :: C.Response C.BodyReader -> IO (C.Response C.BodyReader)
cookieModifier resp = do
  let multiPart = resp ^.. to C.responseCookieJar . getMultipartTok . to (\x -> ("Set-Cookie", x))
  return $ resp { C.responseHeaders = C.responseHeaders resp <> multiPart }  

getMultipartTok :: (Applicative f, Contravariant f) =>
                   (ByteString -> f ByteString)
                -> C.CookieJar -> f C.CookieJar
getMultipartTok = to C.destroyCookieJar . traverse . filtered filterFcn . runFold ((,) <$> Fold (to C.cookie_name) <*> Fold (to C.cookie_value)) . to (\(n, v) -> n <> "=" <> v)
  where
    filterFcn c = C.cookie_name c == "__appianMultipartCsrfToken"
      || C.cookie_name c == "JSESSIONID"
      || C.cookie_name c == "__appianCsrfToken"

clientEnv (HostUrl url) = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https url 443 "")
    where
      settings = TLS.tlsManagerSettings { C.managerModifyResponse = cookieModifier }

newtype LogFilePath = LogFilePath FilePath
  deriving (Show, IsString, Semigroup)

logFilePath = LogFilePath

data LogMode
  = LogStdout
  | LogFile LogFilePath
  deriving Show

runAppianT :: LogMode -> AppianT (LoggingT ClientM) a -> AppianState -> ClientEnv -> Login -> IO (Either ServantError (Either ScriptError a))
-- runAppianT (LogFile (LogFilePath pth)) = runAppianT' (runParallelFileLoggingT pth)
runAppianT LogStdout = runAppianT' runStdoutLoggingT

runAppianT' :: (LoggingT ClientM (Either ScriptError a, AppianState) -> ClientM (Either ScriptError a, AppianState)) -> AppianT (LoggingT ClientM) a -> AppianState -> ClientEnv -> Login -> IO (Either ServantError (Either ScriptError a))
runAppianT' runLogger f appianState env creds = bracket (runClientM' login') (runClientM' . logout') (runClientM' . execFun)
  where
    login' = execAppianT (login creds) appianState
    logout' (Right (_, cookies)) = do
      execAppianT logout cookies
      putStrLn "Successfully logged out!"
    logout' (Left err) = print err
    execFun (Right (_, cookies)) = do
      (res, _) <- execAppianT f cookies
      return res
    execFun (Left err) = throwError . ConnectionError . tshow $  err
    runClientM' act = runClientM (runStdoutLoggingT act) env

    -- Update Functions

buttonUpdate :: Text -> Value -> Either Text Update
buttonUpdate label v = toUpdate <$> btn
  where
    btn = maybeToEither ("Could not locate button " <> tshow label) $ v ^? getButton label

buttonUpdateWith :: (Plated s, AsValue s, AsJSON s) => (Text -> Bool) -> t -> ReifiedMonadicFold m s (Either t Update)
buttonUpdateWith f msg = MonadicFold (failing (getButtonWith f . to toUpdate . to Right) (to (const $ Left msg)))

dropdownUpdate :: Text -> Int -> Value -> Either Text Update
dropdownUpdate label n v = toUpdate <$> (_Right . dfValue .~ n $ dropdown)
  where
    dropdown = maybeToEither ("Could not locate dropdown " <> tshow label) $ v ^? getDropdown label

textUpdate :: Text -> Text -> Value -> Either Text Update
textUpdate label txt v = toUpdate <$> (_Right . tfValue .~ txt $ tf)
  where
    tf = maybeToEither ("Could not find TextField " <> tshow label) $ v ^? getTextField label

pickerUpdate :: (FromJSON b, ToJSON b) => Text -> AppianPickerData b -> Value -> Either Text Update
pickerUpdate label uname v = toUpdate <$> (_Right . pwValue .~ Just (toJSON uname) $ apd)
  where
    apd = maybeToEither ("Could not find PickerField " <> tshow label) $ v ^? getPickerWidget label

paragraphUpdate :: Text -> Text -> Value -> Either Text Update
paragraphUpdate label txt v = toUpdate <$> (_Right . pgfValue .~ txt $ pgf)
  where
    pgf = maybeToEither ("Could not locate ParagraphField " <> tshow label) $ v ^? getParagraphField label

datePickerUpdate :: Text -> AppianDate -> Value -> Either Text Update
datePickerUpdate label date v = toUpdate <$> (_Right . dpwValue .~ date $ dpw)
  where
    dpw = maybeToEither ("Could not locate DatePicker " <> tshow label) $ v ^? getDatePicker label

dynamicLinkUpdate :: Text -> Value -> Either Text Update
dynamicLinkUpdate label v = toUpdate <$> dyl
  where
    dyl = maybeToEither ("Could not locate DynamicLinc " <> tshow label) $ v ^? getDynamicLink label

gridFieldUpdate :: Int -> Value -> Either Text Update
gridFieldUpdate index v = update
  where
    gf = v ^. getGridFieldCell
    ident = bindGet gf $ gfIdentifiers . traverse . ifolded . ifiltered (\i _ -> i == index)
    bindGet mx g = mx >>= \x -> x ^. g . to return
    rUpdate = do
      g <- gf
      ident' <- ident
      return $ toUpdate (gfSelection . _Just . _Selectable . gslSelected %~ (ident' :) $ g)
    update = case rUpdate of
      Error msg -> Left $ "gridFieldUpdate: " <> pack msg
      Success upd -> Right upd

textFieldCidUpdate :: Text -> Text -> Value -> Either Text Update
textFieldCidUpdate cid txt v = toUpdate <$> (_Right . tfValue .~ txt $ tf)
  where
    tf = maybeToEither ("Could not locate TextField with _cId " <> tshow cid) $ v ^? textFieldCid cid

textFieldCid :: (Applicative f, Contravariant f) => Text -> (TextField -> f TextField) -> Value -> f Value
textFieldCid cid = hasKeyValue "_cId" cid . _JSON

dropdownFieldCid :: (Applicative f, Contravariant f, Plated s, AsValue s, AsJSON s) => Text -> (DropdownField -> f DropdownField) -> s -> f s
dropdownFieldCid cid = hasKeyValue "_cId" cid . _JSON

checkboxGroupUpdate :: (Contravariant f, Plated s, AsValue s, AsJSON s, Applicative f) => Text -> [Int] -> Over (->) f s s (Either Text Update) (Either Text Update)
checkboxGroupUpdate label selection = failing (getCheckboxGroup label . to (cbgValue .~ Just selection) . to toUpdate . to Right) (to $ const $ Left $ "Could not find CheckboxField " <> tshow label)

radioButtonUpdate :: (Contravariant f, Plated s, AsValue s, AsJSON s, Applicative f) => Text -> AppianInteger -> Over (->) f s s (Either Text Update) (Either Text Update)
radioButtonUpdate label selection = failing (getRadioButtonField label . to (rdgValue .~ Just selection) . to toUpdate . to Right) (to $ const $ Left $ "Could not find RadioButtonField " <> tshow label)

sendUpdate :: (RunClient m, MonadError ServantError m, MonadThreadId m) => (UiConfig (SaveRequestList Update) -> AppianT m Value) -> Either Text (UiConfig (SaveRequestList Update)) -> AppianT m Value
sendUpdate f update = do
  eRes <- sendUpdate' f update
  case eRes of
    Left v -> throwError v
    Right res -> return res

sendUpdate' :: (RunClient m, MonadError ServantError m, MonadThreadId m) => (UiConfig (SaveRequestList Update) -> AppianT m Value) -> Either Text (UiConfig (SaveRequestList Update)) -> AppianT m (Either ScriptError Value)
sendUpdate' _ (Left msg) = do
  tid <- threadId
  throwError $ BadUpdateError (tshow tid <> ": " <> msg) Nothing
sendUpdate' f (Right x) = do
  v <- f x
  case v ^.. cosmos . key "validations" . _Array . filtered (not . onull) . traverse . key "message" . _String of
    [] -> return $ Right v
    l -> return $ Left $ ValidationsError (l, v, x)

sendUpdates_ :: RapidFire m => (UiConfig (SaveRequestList Update) -> AppianT m Value) -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m (Either ScriptError Value)
sendUpdates_ updateFcn label f v = do
  updates <- lift $ v ^!! runMonadicFold f
  bounds <- use appianBounds
  let errors = lefts updates

  case errors of
    [] -> thinkTimer bounds $ recordTime label $ sendUpdate' updateFcn $ mkUiUpdate (rights updates) v
    l -> do
      tid <- threadId
      throwError $ MissingComponentError (tshow tid <> "\n" <> intercalate "\n" l, v)

sendReportUpdates :: RapidFire m => ReportId -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m Value
sendReportUpdates reportId label f v = do
  eRes <- sendReportUpdates' reportId label f v
  case eRes of
    Left v -> throwError v
    Right res -> return res

sendReportUpdates' :: RapidFire m => ReportId -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m (Either ScriptError Value)
sendReportUpdates' reportId label f v = sendUpdates_ (reportUpdate reportId) label f v

sendUpdates :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m Value
sendUpdates label f v = do
  eRes <- sendUpdates' label f v
  case eRes of
    Left v -> throwError v
    Right res -> return res

sendUpdates' :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m (Either ScriptError Value)
sendUpdates' label f v = do
  taskId <- getTaskId v
  sendUpdates_ (taskUpdate taskId) label f v

deltaUpdate :: (Monad m, MonadError ServantError m, MonadThreadId m) => Value -> Value -> AppianT m Value
deltaUpdate full delta =
    case has (key "ui" . key "#t" . _String . only "UiComponentsDelta") delta of
      False -> return $ trace ("type is " <> delta ^. key "ui" . key "#t" . _String . to unpack)  delta
      True -> handleMissing "Bad update delta?" full $ handleDelta full $ trace ("type is " <> delta ^. key "ui" . key "#t" . _String . to unpack) delta

handleDelta :: Value -> Value -> Maybe Value
handleDelta fullResp delta = do
  let comps = delta ^.. key "ui" . key "modifiedComponents" . plate
  foldlM updateComponent fullResp comps

updateComponent :: Value -> Value -> Maybe Value
updateComponent fullResp componentVal = do
  cid <- componentVal ^? key "_cId" . _String
  return $ (hasKeyValue "_cId" cid .~ componentVal) fullResp

--     -- Utility functions

getTaskId :: (MonadError ServantError m, MonadThreadId m) => Value -> AppianT m TaskId
getTaskId v = handleMissing "taskId" v $ v ^? key "taskId" . _String . to TaskId

diffToMS :: NominalDiffTime -> Int
diffToMS elapsed = fromEnum elapsed `div` 1000000000

resultToEither :: Result a -> Either Text a
resultToEither (Error msg) = Left $ pack msg
resultToEither (Success a) = Right a

handleMissing :: MonadThreadId m => Text -> Value -> Maybe a -> AppianT m a
handleMissing label v Nothing = do
  tid <- threadId
  throwError $ BadUpdateError (tshow tid <> ": " <> label) (Just v)
handleMissing label _ (Just component) = return component

getReportLink :: Text -> Value -> Maybe Text
getReportLink label v = v ^? deep (filtered $ has $ key "title" . _String . only label) . key "links" . _Array . traverse . filtered (has $ key "rel" . _String . only "edit") . key "href" . _String

parseReportId :: Text -> Maybe ReportId
parseReportId = parseLinkId ReportId

getReportId :: (MonadError ServantError m, MonadThreadId m) => Text -> Value -> AppianT m ReportId
getReportId label v = handleMissing ("Cannot find Report: " <> label) v $ (getReportLink label >=> parseReportId) v

landingPageLink :: (AsValue s, Plated s, Applicative f) => Text -> (Text -> f Text) -> s -> f s
landingPageLink label = deep (filtered $ has $ key "values" . key "values" . _Array . traverse . key "#v" . _String . only label) . key "link" . key "uri" . _String

parseLinkId :: (Text -> a) -> Text -> Maybe a
parseLinkId f url = fmap f . handleList . parseReportId' $ url
  where
    handleList [x] = Just x
    handleList _ = Nothing
    parseReportId' = take 1 . reverse . splitElem '/'

parseActionId :: Text -> Maybe ActionId
parseActionId = parseLinkId ActionId

mkUiUpdate :: [Update] -> Value -> Either Text (UiConfig (SaveRequestList Update))
mkUiUpdate [] _ = Left "No updates to send!"
mkUiUpdate saveList v = maybeToEither "Could not create UiUpdate" $ v ^? _JSON . to (id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)) . to (uiUpdates .~ (Just $ SaveRequestList saveList))

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither c = maybe (Left c) Right


-- -- Exception types

-- data BadUpdateException = BadUpdateException
--   { _badUpdateExceptionMsg :: Text
--   , _badUpdateExceptionVal :: Maybe Value
--   }

-- badUpdateExceptionMsg :: Functor f => (Text -> f Text) -> BadUpdateException -> f (BadUpdateException)
-- badUpdateExceptionMsg = lens _badUpdateExceptionMsg upd
--   where
--     upd exc v = exc {_badUpdateExceptionMsg = v}

-- badUpdateExceptionVal :: Functor f => (Maybe Value -> f (Maybe Value)) -> BadUpdateException -> f (BadUpdateException)
-- badUpdateExceptionVal = lens _badUpdateExceptionVal upd
--   where
--     upd exc v = exc {_badUpdateExceptionVal = v}

-- instance Show BadUpdateException where
--   show exc = "BadUpdateException: " <> show (exc ^. badUpdateExceptionMsg)

-- instance Exception BadUpdateException

-- newtype MissingComponentException = MissingComponentException (Text, Value)

-- instance Show MissingComponentException where
--   show (MissingComponentException (name, _)) = "MissingComponentException: Could not find component " <> show name

-- instance Exception MissingComponentException

-- newtype ValidationsException = ValidationsException { _validationsExc :: ([Text], Value, UiConfig (SaveRequestList Update)) }

-- validationsExc :: Functor f => (([Text], Value, UiConfig (SaveRequestList Update))-> f (([Text], Value, UiConfig (SaveRequestList Update)))) -> ValidationsException -> f (ValidationsException)
-- validationsExc = lens _validationsExc f
--   where
--     f exc tpl = exc { _validationsExc = tpl }

-- instance Show ValidationsException where
--   show (ValidationsException validations) = "ValidationsException: \n" <> intercalate "\n" (fmap unpack $ validations ^. _1)

-- instance Exception ValidationsException

-- -- newtype ServerException = ServerException { _serverExc :: (SomeException, UiConfig (SaveRequestList Update)) }

-- -- instance Show ServerException where
-- --   show (ServerException exc) = "ServerException: " <> show (exc ^. _1)

-- -- instance Exception ServerException

-- -- newtype ClientException = ClientException Text

-- -- instance Show ClientException where
-- --   show (ClientException exc) = "ClientException: " <> show (exc)

-- -- instance Exception ClientException

logServantError :: (MonadLogger m, MonadTime m, MonadDelay m, MonadThreadId m) => UTCTime -> Text -> ServantError -> m ()
logServantError start label (FailureResponse resp) = do
  end <- currentTime
  logResponse start end label $ responseStatusCode resp
logServantError start label (DecodeFailure t resp) = do
  end <- currentTime
  let status = responseStatusCode resp
  logResponse start end label (status { statusMessage = "Decode Failure!" })
logServantError start label (UnsupportedContentType mt resp) = do
  end <- currentTime
  let status = responseStatusCode resp
  logResponse start end label (status { statusMessage = "Unsupported Content Type" } )
logServantError start label (InvalidContentTypeHeader resp) = do
  end <- currentTime
  logResponse start end label $ responseStatusCode resp
logServantError start label (ConnectionError msg) = do
  end <- currentTime
  logResponse start end label (status400 { statusMessage = encodeUtf8 msg })

logScriptError :: (MonadLogger m, MonadTime m, MonadDelay m, MonadThreadId m) => UTCTime -> Text -> ScriptError -> m ()
logScriptError start label err@(ValidationsError (errMsgs, _, _)) = do
  end <- currentTime
  logResponse start end label (status400 { statusMessage = encodeUtf8 $ tshow errMsgs })
logScriptError start label err@(MissingComponentError (msg, _)) = do
  end <- currentTime
  logResponse start end label (status400 { statusMessage = encodeUtf8 msg })
logScriptError start label err@(BadUpdateError msg _) = do
  end <- currentTime
  logResponse start end label (status400 { statusMessage = encodeUtf8 msg })

-- -- dispatchAppianError :: (MonadLogger m, MonadError AppianError m, MonadTime m) => UTCTime -> Text -> AppianError -> m a
-- -- dispatchAppianError start label err@(ServerError servantError) = logServantError start label servantError >> throwError err
-- -- dispatchAppianError start label err@(ScriptError scriptError) = logScriptError start label scriptError >> throwError err

logServantError_ start label err = logServantError start label err >> throwError err

logScriptError_ start label err = trace "Caught Script Error!" $ logScriptError start label err >> throwError err

logAppianError start label f = (f `catchServerError` (logServantError_ start label)) `catchError` logScriptError_ start label

recordTime :: (MonadTime m, MonadLogger m, MonadError ServantError m, MonadDelay m, MonadThreadId m) => Text -> AppianT m a -> AppianT m a
recordTime label f = do
  start <- currentTime
  res <- logAppianError start label f
  end <- currentTime
  logResponse start end label status200
  return res

logResponse :: (MonadLogger m, MonadDelay m, MonadThreadId m) => UTCTime -> UTCTime -> Text -> Status -> m ()
logResponse start end label status = do
  tid <- threadId
  let textList =
        [ toUrlPiece (1000 * utcTimeToPOSIXSeconds start)
        , tshow (diffToMS elapsed)
        , label
        , tshow (statusCode status)
        , decodeUtf8 (statusMessage status)
        , tshow tid
        ]
      encoded = Csv.encode $ [textList]
  msg <- case stripSuffix "\r\n" encoded of
    Nothing -> case stripSuffix "\n" encoded of
                    Nothing -> fail "Could not stript the newline off of the log message for some reason"
                    Just bs -> return bs
    Just bs -> return bs
  logInfoN . toStrict . decodeUtf8 $ msg
  where
    elapsed = diffUTCTime end start

thinkTimer :: (MonadDelay m, MonadRandom m, MonadDelay m) => Bounds -> m a -> m a
thinkTimer bounds f = do
    secs <- sample $ uniform (bounds ^. lowerBound) (bounds ^. upperBound)
    delay (secs * 1000000)
    f

data ScriptError
  = ValidationsError ([Text], Value, UiConfig (SaveRequestList Update))
  | MissingComponentError (Text, Value)
  | BadUpdateError
    { _badUpdateErrorMsg :: Text
    , _badUpdateErrorVal :: Maybe Value
    }
  | BadPagingError ThreadId

instance Show ScriptError where
  show (ValidationsError (msgs, _, _)) = "ValidationsError: " <> (intercalate "\n" $ fmap unpack msgs)
  show (MissingComponentError (msg, _)) = "MissingComponentError: " <> unpack msg
  show (BadUpdateError msg _) = "BadUpdateError: " <> unpack msg
  show (BadPagingError tid) = show tid <> ": It looks like paging is not working correctly!"

type AppianT = AppianET ScriptError

type Appian = AppianT (LoggingT ClientM)

-- data AppianError
--   = ScriptError ScriptError
--   | ServerError ServantError

-- makePrisms ''AppianError
makePrisms ''ScriptError
makeLenses ''ScriptError

newtype RecordName = RecordName Text
    deriving (Show, Eq, IsString)

  -- Stateful versions
-- Same as recordsTab but keeps the JSON Value in the state of the AppianT transformer
recordsTab' :: RapidFire m => AppianT m ()
recordsTab' = do
  v <- recordsTab
  assign appianValue v

-- Same as viewRecord but keeps the JSON Value in the state of the AppianT transformer
viewRecord' :: RapidFire m => RecordId -> AppianT m ()
viewRecord' rid = do
  v <- viewRecord rid
  assign appianValue v

-- Allows one to view a record by using the label displayed in the Records tab.
viewRecordByName :: RapidFire m => RecordName -> AppianT m ()
viewRecordByName (RecordName recordName) = do
    recordsTab'
    mRid <- usesValue (^? hasKeyValue "title" recordName . key "link" . key "value" . key "urlstub" . _String . to RecordId)
    case mRid of
        Nothing -> fail $ "Could not find recordId for the record " <> show recordName
        Just rid -> viewRecord' rid
        
-- Same as relatedActionEx but keeps the JSON Value in the state of the AppianT transformer
executeRelatedAction' :: RapidFire m => Text -> RecordRef -> AppianT m ()
executeRelatedAction' label rref = do
    v <- use appianValue
    v' <- executeRelatedAction label rref v
    assign appianValue v'
    
-- Same as handleMissing but uses the JSON Value in the state of the AppianT transformer
handleMissing' :: RapidFire m => Text -> Maybe a -> AppianT m a
handleMissing' message mVal = do
    v <- use appianValue
    handleMissing message v mVal

-- Takes a RecordRef and stores the JSON Value of the record in the AppianT transformer
viewRelatedActions' :: RapidFire m => RecordRef -> AppianT m ()
viewRelatedActions' rref = do
    v <- use appianValue
    (_, v') <- viewRelatedActions v rref
    assign appianValue v'

executeRelatedAction :: RapidFire m => Text -> RecordRef -> Value -> AppianT m Value
executeRelatedAction action recordId val = do
  aid <- handleMissing ("could not find actionId for " <> tshow action) val $ val ^? getRelatedActionId action
  relatedActionEx recordId aid

viewRelatedActions :: RapidFire m => Value -> RecordRef -> AppianT m (RecordRef, Value)
viewRelatedActions v recordRef = do
  let ref = recordRef
  v' <- viewRecordDashboard ref (Dashboard "summary")
  dashboard <- handleMissing "Related Actions Dashboard" v' $ v' ^? getRecordDashboard "Related Actions"
  v'' <- viewRecordDashboard ref dashboard
  return (recordRef, v'')

sendUpdates1 :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
sendUpdates1 msg fold = do
  previousVal <- use appianValue
  newVal <- sendUpdates msg fold previousVal
  res <- deltaUpdate previousVal newVal
  assign appianValue res

sendUpdates1' :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m (Either ScriptError ())
sendUpdates1' msg fold = do
  previousVal <- use appianValue
  eNewVal <- sendUpdates' msg fold previousVal
  case eNewVal of
    Left ve -> return $ Left ve
    Right newVal -> do
      res <- deltaUpdate previousVal newVal
      Right <$> assign appianValue res

type Updater m = (Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m Value)

forGridRows1_ :: RapidFire m => Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m ()) -> AppianT m ()
forGridRows1_ = forGridRowsWith1_ (const True)

forGridRowsWith1_ :: RapidFire m => (Int -> Bool) -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m ()) -> AppianT m ()
forGridRowsWith1_ continueFcn updateFcn colFcn fold f = do
  v <- use appianValue
  gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
  loop (gf ^. gfTotalCount) 0
    where
      loop total idx = do
        val <- use appianValue
        case (idx < total && continueFcn idx) of
          False -> assign appianValue val
          True -> do
            (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
            res <- deltaUpdate val val'
            assign appianValue res
            f b gf
            loop total (idx + 1)

getPagedItem :: RapidFire m => Updater m -> (GridField a -> Vector b) -> Int -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m (b, GridField a, Value)
getPagedItem updateFcn colFcn idx fold val = do
  gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
  pi <- handleMissing "GridField is not pageable" val $ gf ^? pagingInfo

  let (startIdx, (RowIndex rowIdx)) = getRowIdx idx pi

  val' <- getPage updateFcn fold startIdx val
  gf <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
  res <- handleMissing ("Row idx: " <> tshow rowIdx) val' $ flip index rowIdx $ colFcn gf
  return $ (res, gf, val')

newtype RowIndex = RowIndex Int
  deriving (Show, Eq)

newtype StartIndex = StartIndex Int
  deriving (Show, Eq)

pagingInfo :: Applicative f => (PagingInfo -> f PagingInfo) -> GridField a -> f (GridField a)
pagingInfo = gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable

getRowIdx :: Int -> PagingInfo -> (StartIndex, RowIndex)
getRowIdx idx pi = (startIdx, rowIdx)
  where
    pageNo = idx `div` pi ^. pgIBatchSize
    startIdx
      | pi ^. pgIBatchSize == (-1) = StartIndex 1
      | otherwise = StartIndex $ pageNo * pi ^. pgIBatchSize + 1
    rowIdx = RowIndex (idx `rem` pi ^. pgIBatchSize)

         -- Fetches the page with the given start index. Will throw an
         -- error if the server responds with a start index that is
         -- not the same as the given start index.
getPage :: RapidFire m => Updater m -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> StartIndex -> Value -> AppianT m Value
getPage updateFcn fold idx val = do
  gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
  case maybe False (== idx) (gf ^? pagingInfo . pgIStartIndex . to StartIndex) of
    True -> return val
    False -> do
      let gf' = setStartIndex idx gf
          msg = "Getting page with startIndex: " <> tshow idx

      val' <- updateFcn msg (MonadicFold $ to $ const $ Right $ toUpdate gf') val
      gf'' <- handleMissing ("No GridField on the page with startIndex: " <> tshow idx) val' =<< (val' ^!? runMonadicFold fold)
      case checkPaging idx gf'' of
        True -> return val'
        False -> do
          tid <- threadId
          throwError $ BadPagingError tid

setStartIndex :: StartIndex -> GridField a -> GridField a
setStartIndex (StartIndex idx) = pagingInfo . pgIStartIndex .~ idx

checkPaging :: StartIndex -> GridField a -> Bool
checkPaging (StartIndex idx) gf = maybe False (== idx) si
  where
    si = gf ^? pagingInfo . pgIStartIndex

