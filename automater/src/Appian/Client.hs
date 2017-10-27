{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Appian.Client
  ( module Appian.Client
  , Servant.Client.Core.RunClient
  , Control.Monad.Logger.MonadLogger
  ) where

import Servant.API
import Servant.Client hiding (responseStatus, HasClient, Client)
import Servant.Client.Core hiding (ServantError, BaseUrl, Https)
import Control.Lens hiding (cons)
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

type HomepageAPI = Get '[HTML] (Headers '[Header "Set-Cookie" Text] NoContent)
type LoginAPI = "suite" :> "auth" :> QueryParam "appian_environment" Text :> QueryParam "un" Text :> QueryParam "pw" Text :> QueryParam "X-APPIAN-CSRF-TOKEN" Text
  :> Header "User-Agent" UserAgent :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] ByteString)

type RecordsTab = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "view" :> "all" :> Header "User-Agent" UserAgent :> Get '[AppianApplication] Value

type LogoutAPI = "suite" :> "logout" :> Get '[HTML] ByteString

type ReportsTab = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> "reports" :> Get '[AtomApplication, JSON] Value

type TasksTab = "suite" :> "api" :> "feed" :> "tempo" :> QueryParam "m" Text :> QueryParam "t" Text :> QueryParam "s" Text
                :> QueryParam "defaultFacets" Text :> QueryParam "b" UTCTime :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] Value

type ViewRecord = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "type" :> Capture "recordId" RecordId :> "view" :> "all" :> Get '[AppianApplication] Value

type ViewRecordDashboard = "suite" :> "rest" :> "a" :> "record" :> "latest" :> Capture "recordRef" RecordRef :> "dashboards" :> Capture "dashboard" Dashboard :> Get '[InlineSail] Value

type EditReport = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> Capture "repordId" ReportId :> "view" :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[AppianTVUI] Value

type ReportUpdate update = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text
  :> Capture "recordId" ReportId :> "view" :> ReqBody '[AppianTV] (UiConfig update) :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type TaskUpdate update = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Header "X-Appian-Features" Text :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text
  :> Capture "taskId" TaskId :> "form" :> ReqBody '[AppianTV] (UiConfig update) :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type TaskAccept = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Capture "taskId" TaskId :> "accept" :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[JSON] NoContent

type LandingPageAction = "suite" :> "api" :> "tempo" :> "open-a-case" :> "action" :> Capture "actionId" ActionId :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] ProcessModelId

type LandingPageActionEx = "suite" :> "rest" :> "a" :> "model" :> "latest" :> Capture "processModelId" ProcessModelId :> ReqBody '[EmptyAppianTV] EmptyAppianTV :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type RelatedActionEx = "suite" :> "rest" :> "a" :> "record" :> "latest" :> Capture "recordRef" RecordRef :> "action" :> Capture "actionId" ActionId :> ReqBody '[EmptyAppianTV] EmptyAppianTV
   :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Post '[AppianTVUI] Value

type ActionsTab = "suite" :> "api" :> "tempo" :> "open-a-case" :> "available-actions" :> Header "X-Appian-Features" Text :> Header "X-APPIAN-CSRF-TOKEN" Text :> Get '[JSON] Value

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

login :: RunClient m => Login -> AppianT m (Headers '[Header "Set-Cookie" Text] ByteString)
login (Login un pw) = do
  res <- navigateSite
  let cj = res ^.. getCookies & Cookies
  put cj
  res' <- login_ (Just "TEMPO") (Just un) (Just pw) (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8) (Just defUserAgent)
  put $ Cookies $ (cj ^.. unCookies . traverse . filtered removeNew) <> res' ^.. getCookies
  return res'
  where
    login_ = toClient Proxy (Proxy :: Proxy LoginAPI)
    removeNew (n, _) = not (n == "JSESSIONID" || n == "__appianCsrfToken")

recordsTab :: (RunClient m, MonadLogger m) => AppianT m Value
recordsTab = logDebugN "Viewing records tab now!" >> recordsTab_ (Just defUserAgent)
  where
    recordsTab_ = toClient Proxy (Proxy :: Proxy RecordsTab)

reportsTab :: RunClient m => AppianT m Value
reportsTab = toClient Proxy (Proxy :: Proxy ReportsTab)

logout :: RunClient m => AppianT m ByteString
logout = toClient Proxy (Proxy :: Proxy LogoutAPI)

tasksTab :: RunClient m => Maybe UTCTime -> AppianT m Value
tasksTab mUtcTime = do
  cj <- get
  tasksTab_ (Just "menu-tasks") (Just "t") (Just "pt") (Just "%5Bstatus-open%5D") mUtcTime (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    tasksTab_ = toClient Proxy (Proxy :: Proxy TasksTab)

viewRecord :: RunClient m => RecordId -> AppianT m Value
viewRecord rid = viewRecord_ rid
  where
    viewRecord_ = toClient Proxy (Proxy :: Proxy ViewRecord)

editReport :: RunClient m => ReportId -> AppianT m Value
editReport rid = do
  cj <- get
  toClient Proxy (Proxy :: Proxy EditReport) rid (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)

reportUpdate :: (RunClient m, ToJSON update) => ReportId -> UiConfig update -> AppianT m Value
reportUpdate rid upd = do
  cj <- get
  reportUpdate_ (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO") rid upd (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      reportUpdate_ = toClient Proxy (Proxy :: ToJSON update => Proxy (ReportUpdate update))

taskUpdate :: (RunClient m, ToJSON update) => TaskId -> UiConfig update -> AppianT m Value
taskUpdate tid upd = do
  cj <- get
  taskUpdate_ (Just ("ceebc" :: Text)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO") tid upd (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    taskUpdate_ = toClient Proxy (Proxy :: ToJSON update => Proxy (TaskUpdate update))

taskAccept :: RunClient m => TaskId -> AppianT m NoContent
taskAccept tid = do
  cj <- get
  taskAccept_ tid (Just "e4bc") (Just "stateful") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
  where
    taskAccept_ = toClient Proxy (Proxy :: Proxy TaskAccept)

landingPageAction :: RunClient m => ActionId -> AppianT m ProcessModelId
landingPageAction aid = do
  cj <- get
  landingPageAction_ aid (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      landingPageAction_ = toClient Proxy (Proxy :: Proxy LandingPageAction)

landingPageActionEx :: RunClient m => ProcessModelId -> AppianT m Value
landingPageActionEx pid = do
  cj <- get 
  landingPageActionEx_ pid EmptyAppianTV (Just "ceebc") (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      landingPageActionEx_ = toClient Proxy (Proxy :: Proxy LandingPageActionEx)

viewRecordDashboard :: RunClient m => RecordRef -> Dashboard -> AppianT m Value
viewRecordDashboard = toClient Proxy (Proxy :: Proxy ViewRecordDashboard)

relatedActionEx :: RunClient m => RecordRef -> ActionId -> AppianT m Value
relatedActionEx ref aid = do
  cj <- get
  relatedActionEx_ ref aid EmptyAppianTV (Just "ceebc") (Just "stateful") (Just "TEMPO") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      relatedActionEx_ = toClient Proxy (Proxy :: Proxy RelatedActionEx)

actionsTab :: RunClient m => AppianT m Value
actionsTab = do
  cj <- get
  actionsTab_ (Just "e4bc") (cj ^? unCookies . traverse . getCSRF . _2 . to decodeUtf8)
    where
      actionsTab_ = toClient Proxy (Proxy :: Proxy ActionsTab)

    -- Environments

test3ClientEnvDbg = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https "portal-test3.appiancloud.com" 443 "")
    where
      reqLoggerFunc req = print req >> return req
      settings = TLS.tlsManagerSettings { C.managerModifyRequest = reqLoggerFunc
                                        , C.managerModifyResponse = respLoggerFunc
                                        }
      respLoggerFunc resp = do
        print (C.responseStatus resp)
        print (C.responseHeaders resp)
        print (C.responseCookieJar resp)
        cookieModifier resp

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

preprodClientEnv = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https "portal-preprod.usac.org" 443 "")
    where
      settings = TLS.tlsManagerSettings { C.managerModifyResponse = cookieModifier }

test3ClientEnv = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https "portal-test3.appiancloud.com" 443 "")
    where
      settings = TLS.tlsManagerSettings { C.managerModifyResponse = cookieModifier }

test2ClientEnv = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https "portal-test2.appiancloud.com" 443 "")
    where
      settings = TLS.tlsManagerSettings { C.managerModifyResponse = cookieModifier }

forumClientEnv = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https "forum.appian.com" 443 "")
    where
      settings = TLS.tlsManagerSettings { C.managerModifyResponse = cookieModifier }

runAppianT :: AppianT (LoggingT ClientM) a -> ClientEnv -> Login -> IO (Either SomeException a)
runAppianT f env creds = bracket (runClientM' login') (runClientM' . logout') (runClientM' . execFun)
  where
    login' = execAppianT (login creds) mempty
    logout' (Right (_, cookies)) = do
      execAppianT logout cookies
      putStrLn "Successfully logged out!"
    logout' (Left err) = print err
    execFun (Right (_, cookies)) = do
      (res, _) <- runStderrLoggingT $ execAppianT f cookies
      return res
    execFun (Left err) = throwM err
    runClientM' act = do
      res <- runClientM act env
      return $ bimap toException id $ res

    -- Update Functions

buttonUpdate :: Text -> Value -> Either Text Update
buttonUpdate label v = toUpdate <$> btn
  where
    btn = maybeToEither ("Could not locate button " <> tshow label) $ v ^? getButton label

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

checkboxGroupUpdate :: (Contravariant f, Plated s, AsValue s, AsJSON s, Applicative f) => Text -> [Int] -> Over (->) f s s (Either Text Update) (Either Text Update)
checkboxGroupUpdate label selection = failing (getCheckboxGroup label . to (cbgValue .~ Just selection) . to toUpdate . to Right) (to $ const $ Left $ "Could not find CheckboxField " <> tshow label)

radioButtonUpdate :: (Contravariant f, Plated s, AsValue s, AsJSON s, Applicative f) => Text -> AppianInteger -> Over (->) f s s (Either Text Update) (Either Text Update)
radioButtonUpdate label selection = failing (getRadioButtonField label . to (rdgValue .~ Just selection) . to toUpdate . to Right) (to $ const $ Left $ "Could not find RadioButtonField " <> tshow label)

sendUpdate :: (RunClient m, MonadThrow m, MonadCatch m) => (UiConfig (SaveRequestList Update) -> AppianT m Value) -> Either Text (UiConfig (SaveRequestList Update)) -> AppianT m Value
sendUpdate f update = do
  eRes <- sendUpdate' f update
  case eRes of
    Left v -> throwM v
    Right res -> return res

sendUpdate' :: (RunClient m, MonadThrow m, MonadCatch m) => (UiConfig (SaveRequestList Update) -> AppianT m Value) -> Either Text (UiConfig (SaveRequestList Update)) -> AppianT m (Either ValidationsException Value)
sendUpdate' _ (Left msg) = throwM $ BadUpdateException msg Nothing
sendUpdate' f (Right x) = do
  eV <- tryAny $ f x
  case eV of
    Left exc -> throwM $ ServerException (exc, x)
    Right v ->
      case v ^.. cosmos . key "validations" . _Array . filtered (not . onull) . traverse . key "message" . _String of
        [] -> return $ Right v
        l -> return $ Left $ ValidationsException (l, v, x)

sendUpdates_ :: (RunClient m, MonadIO m, MonadLogger m, MonadCatch m) => (UiConfig (SaveRequestList Update) -> AppianT m Value) -> Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT m (Either ValidationsException Value)
sendUpdates_ updateFcn label f v = do
  updates <- liftIO $ v ^!! runMonadicFold f
  let errors = lefts updates
      -- updates = v ^.. runFold f
  case errors of
    [] -> do
      start <- liftIO $ getCurrentTime
      res <- sendUpdate' updateFcn $ mkUiUpdate (rights updates) v
      end <- liftIO $ getCurrentTime
      let elapsed = diffUTCTime end start
      logInfoN $ intercalate ","
        [ toUrlPiece (1000 * utcTimeToPOSIXSeconds start)
        , tshow (diffToMS elapsed)
        , label
        , "200"
        ]
      -- atomically $ writeTChan logChan $ Msg $ intercalate ","
      --   [ toUrlPiece (1000 * utcTimeToPOSIXSeconds start)
      --   , tshow (diffToMS elapsed)
      --   , label
      --   , "200"
      --   ]
      return res
    l -> throwM $ MissingComponentException (intercalate "\n" l, v)

sendReportUpdates :: (RunClient m, MonadThrow m, MonadIO m, MonadLogger m, MonadCatch m) => ReportId -> Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT m Value
sendReportUpdates reportId label f v = do
  eRes <- sendReportUpdates' reportId label f v
  case eRes of
    Left v -> throwM v
    Right res -> return res

sendReportUpdates' :: (RunClient m, MonadThrow m, MonadIO m, MonadLogger m, MonadCatch m) => ReportId -> Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT m (Either ValidationsException Value)
sendReportUpdates' reportId label f v = sendUpdates_ (reportUpdate reportId) label f v

sendUpdates :: (RunClient m, MonadThrow m, MonadIO m, MonadLogger m, MonadCatch m) => Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT m Value
sendUpdates label f v = do
  eRes <- sendUpdates' label f v
  case eRes of
    Left v -> throwM v
    Right res -> return res

sendUpdates' :: (RunClient m, MonadThrow m, MonadIO m, MonadLogger m, MonadCatch m) => Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT m (Either ValidationsException Value)
sendUpdates' label f v = do
  taskId <- getTaskId v
  sendUpdates_ (taskUpdate taskId) label f v

    -- Utility functions

getTaskId :: MonadThrow m => Value -> AppianT m TaskId
getTaskId v = handleMissing "taskId" v $ v ^? key "taskId" . _String . to TaskId

diffToMS :: NominalDiffTime -> Int
diffToMS elapsed = fromEnum elapsed `div` 1000000000

resultToEither :: Result a -> Either Text a
resultToEither (Error msg) = Left $ pack msg
resultToEither (Success a) = Right a

handleMissing :: MonadThrow (AppianT m) => Text -> Value -> Maybe a -> AppianT m a
handleMissing label v Nothing = throwM $ BadUpdateException label (Just v)
handleMissing label _ (Just component) = return component

getReportLink :: Text -> Value -> Maybe Text
getReportLink label v = v ^? deep (filtered $ has $ key "title" . _String . only label) . key "links" . _Array . traverse . filtered (has $ key "rel" . _String . only "edit") . key "href" . _String

parseReportId :: Text -> Maybe ReportId
parseReportId = parseLinkId ReportId

getReportId :: MonadThrow m => Text -> Value -> AppianT m ReportId
getReportId label v = handleMissing label v $ (getReportLink label >=> parseReportId) v

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


-- Exception types

data BadUpdateException = BadUpdateException
  { _badUpdateExceptionMsg :: Text
  , _badUpdateExceptionVal :: Maybe Value
  }

badUpdateExceptionMsg :: Functor f => (Text -> f Text) -> BadUpdateException -> f (BadUpdateException)
badUpdateExceptionMsg = lens _badUpdateExceptionMsg upd
  where
    upd exc v = exc {_badUpdateExceptionMsg = v}

badUpdateExceptionVal :: Functor f => (Maybe Value -> f (Maybe Value)) -> BadUpdateException -> f (BadUpdateException)
badUpdateExceptionVal = lens _badUpdateExceptionVal upd
  where
    upd exc v = exc {_badUpdateExceptionVal = v}

instance Show BadUpdateException where
  show exc = "BadUpdateException: " <> show (exc ^. badUpdateExceptionMsg)

instance Exception BadUpdateException

newtype MissingComponentException = MissingComponentException (Text, Value)

instance Show MissingComponentException where
  show (MissingComponentException (name, _)) = "MissingComponentException: Could not find component " <> show name

instance Exception MissingComponentException

newtype ValidationsException = ValidationsException { _validationsExc :: ([Text], Value, UiConfig (SaveRequestList Update)) }

validationsExc :: Functor f => (([Text], Value, UiConfig (SaveRequestList Update))-> f (([Text], Value, UiConfig (SaveRequestList Update)))) -> ValidationsException -> f (ValidationsException)
validationsExc = lens _validationsExc f
  where
    f exc tpl = exc { _validationsExc = tpl }
-- makeLenses ''ValidationsException

instance Show ValidationsException where
  show (ValidationsException validations) = "ValidationsException: \n" <> intercalate "\n" (fmap unpack $ validations ^. _1)

instance Exception ValidationsException

newtype ServerException = ServerException { _serverExc :: (SomeException, UiConfig (SaveRequestList Update)) }

instance Show ServerException where
  show (ServerException exc) = "ServerException: " <> show (exc ^. _1)

instance Exception ServerException

newtype ClientException = ClientException Text

instance Show ClientException where
  show (ClientException exc) = "ClientException: " <> show (exc)

instance Exception ClientException

