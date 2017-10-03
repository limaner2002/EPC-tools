{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Appian.Client where

import ClassyPrelude
import Servant
import Servant.Client hiding (responseStatus)
import Network.HTTP.Client.TLS
import Network.HTTP.Client ( newManager, defaultManagerSettings, managerModifyRequest
                           , managerModifyResponse, responseStatus, responseHeaders
                           , responseCookieJar, CookieJar
                           )
import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Reified
import qualified Web.Cookie as WC
import Network.HTTP.Media ((//), (/:))
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Data.Time (diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO.Unsafe (unsafePerformIO)

       -- Horrible hack that will be removed once generalization of
       -- ClientM is included in servant-client which will allow me to
       -- replace this with monad-logger
logChan :: TChan LogMessage
logChan = unsafePerformIO newTChanIO

data LogMessage
  = Msg Text
  | Done
  deriving Show

type HomepageAPI = Get '[HTML] (Headers '[Header "Set-Cookie" Text] NoContent)
type TestAPI = Get '[HTML] CookieJar
type LoginAPI = "suite" :> "auth" :> QueryParam "appian_environment" Text :> Login :> LoginCj :> Post '[HTML] CookieJar
type LogoutAPI = "suite" :> "logout" :> CookieJar :> Get '[HTML] ByteString
type RecordsTab = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "view" :> "all" :> CookieJar :> Get '[AppianApplication] Value
type ViewRecord = "suite" :> "rest" :> "a" :> "applications" :> "latest" :> "tempo" :> "records" :> "type" :> RecordId :> "view" :> "all" :> CookieJar :> Get '[AppianApplication] Value
type ViewRecordEntry = "suite" :> "rest" :> "a" :> "record" :> "latest" :> PathPiece RecordRef :> "dashboards" :> PathPiece Dashboard :> CookieJar :> Get '[InlineSail] Value
-- type RecordActions = "suite" :> "rest" :> "a" :> "record" :> "latest" :> PathPiece RecordRef :> "dashboards" :> PathPiece Dashboard :> AppianCsrfToken :> Get '[InlineSail] Value
type TasksTab = "suite" :> "api" :> "feed" :> TaskParams :> QueryParam "b" UTCTime :> AppianCsrfToken :> Get '[JSON] Value
type AcceptTask = "suite" :> "rest" :> "a" :> "task" :> "latest" :> PathPiece TaskId :> "accept" :> AppianCsrfToken :> Post '[JSON] NoContent
type TaskStatus = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> PathPiece TaskId :> "status" :> ReqBody '[PlainText] Text :> AppianCsrfToken :> Put '[AppianTVUI] Value
type TaskAttributes = "suite" :> "rest" :> "a" :> "task" :> "latest" :> PathPiece TaskId :> "attributes" :> AppianCsrfToken :> Get '[JSON] Value
type TaskUpdate update = "suite" :> "rest" :> "a" :> "task" :> "latest" :> Header "X-Appian-Features" Text :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text
  :> PathPiece TaskId :> "form" :> ReqBody '[AppianTV] (UiConfig update) :> AppianCsrfToken :> Post '[AppianTVUI] Value
type ReportsTab = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> "reports" :> AppianCsrfToken :> Get '[AtomApplication, JSON] Value
type EditReport = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> PathPiece ReportId :> "view" :> AppianCsrfToken :> Get '[AppianTVUI] Value
type UIUpdate update = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> Header "Accept-Language" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text
  :> PathPiece ReportId :> "view" :> ReqBody '[AppianTV] (UiConfig update) :> AppianCsrfToken :> Post '[AppianTVUI] Value
-- type RelatedActionEx = Header "X-Appian-Features" Text :> Url :> EmptyAppianTV :> AppianCsrfToken :> Post '[AppianTVUI] Value
type AvailableActions = "suite" :> "api" :> "tempo" :> "open-a-case" :> "available-actions" :> Header "X-Appian-Features" Text :> AppianCsrfToken :> Get '[JSON] Value
type LandingPageAction = "suite" :> "api" :> "tempo" :> "open-a-case" :> "action" :> PathPiece ActionId :> AppianCsrfToken :> Get '[JSON] ProcessModelId
type LandingPageActionEx = "suite" :> "rest" :> "a" :> "model" :> "latest" :> PathPiece ProcessModelId :> EmptyAppianTV :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> AppianCsrfToken :> Post '[AppianTVUI] Value
type RelatedActionEx = "suite" :> "rest" :> "a" :> "record" :> "latest" :> PathPiece RecordRef :> "action" :> PathPiece ActionId :> EmptyAppianTV
   :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> AppianCsrfToken :> Post '[AppianTVUI] Value

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
  :<|> RelatedActionEx
  :<|> AvailableActions
  :<|> LandingPageAction
  :<|> LandingPageActionEx

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
  :<|> relatedActionEx_
  :<|> actionsTab_
  :<|> landingPageAction_
  :<|> landingPageActionEx_
  = client api

recordsTab = AppianT recordsTab_
viewRecordDashboard e dashboard = AppianT (viewRecordEntry_ e dashboard)
viewRecord r = AppianT (viewRecord_ r)
viewRecordEntry e = AppianT (viewRecordEntry_ e (PathPiece $ Dashboard "summary"))
tasksTab mUtcTime = AppianT (tasksTab_ mUtcTime)
taskAttributes tid = AppianT (taskAttributes_ tid)
acceptTask tid = AppianT (acceptTask_ tid)
taskStatus tid = AppianT (taskStatus_ (Just ("ceebc" :: Text)) (Just "stateful") (Just "TEMPO") tid ("accepted" :: Text))
reportsTab = AppianT reportsTab_
editReport rid = AppianT (editReport_ rid)
recordActions rid = AppianT (viewRecordEntry_ rid (PathPiece $ Dashboard "actions"))
relatedActionEx rid aid = AppianT (relatedActionEx_ rid aid (Just "ceebc") (Just "stateful") (Just "TEMPO"))
actionsTab = AppianT (actionsTab_ (Just "e4bc"))
landingPageAction aid = AppianT (landingPageAction_ aid)
landingPageActionEx pid = AppianT (landingPageActionEx_ pid (Just "ceebc") (Just "stateful") (Just "TEMPO"))

taskUpdate :: ToJSON update => PathPiece TaskId -> UiConfig update -> Appian Value
taskUpdate tid ui = AppianT (taskUpdate_ tid ui)
  where
    taskUpdate_ = client (Proxy :: ToJSON update => Proxy (TaskUpdate update)) (Just ("ceebc" :: Text)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO")

uiUpdate :: ToJSON update => PathPiece ReportId -> UiConfig update -> Appian Value
uiUpdate rid ui = AppianT (uiUpdate_ rid ui)
  where
    uiUpdate_ = client (Proxy :: ToJSON update => Proxy (UIUpdate update)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO")

getCurrentTaskIds :: Appian [Text]
getCurrentTaskIds = do
  v <- tasksTab Nothing
  return $ v ^.. getTaskIds

getTaskIds :: (Contravariant f, AsValue s, Plated s, Applicative f) => (Text -> f Text) -> s -> f s
getTaskIds = deep (key "entries") . _Array . traverse . key "id" . _String . to (stripPrefix "t-") . _Just

getRelatedActionId :: (Contravariant f, Applicative f, Plated s, AsValue s) => Text -> (ActionId -> f ActionId) -> s -> f s
getRelatedActionId action = hasKeyValue "title" action . deep (hasKeyValue "title" "Execute related action") . key "href" . _String . to (lastMay . splitElem '/') . traverse . to ActionId

getCloseButton :: (Contravariant f, AsJSON s, AsValue s, Plated s, Applicative f) => (ButtonWidget -> f ButtonWidget) -> s -> f s
getCloseButton = deep (filtered $ has $ key "label" . _String . filtered (\label -> label == "Close" || label == "Cancel" || label == "Reject" || label == "Discard Form" || label == "Discard Request" || label == "Delete Form")) . _JSON

getReportLink :: Text -> Value -> Maybe Text
getReportLink label v = v ^? deep (filtered $ has $ key "title" . _String . only label) . key "links" . _Array . traverse . filtered (has $ key "rel" . _String . only "edit") . key "href" . _String

parseReportId :: Text -> Maybe ReportId
parseReportId = parseLinkId ReportId

parseActionId :: Text -> Maybe ActionId
parseActionId = parseLinkId ActionId

parseLinkId :: (Text -> a) -> Text -> Maybe a
parseLinkId f url = fmap f . handleList . parseReportId' $ url
  where
    handleList [x] = Just x
    handleList _ = Nothing
    parseReportId' = take 1 . reverse . splitElem '/'

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

sendSelection :: PathPiece ReportId -> Text -> Int -> Value -> Appian Value
sendSelection = sendSelection' uiUpdate

sendSelection' :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> Int -> Value -> Appian Value
sendSelection' f pathPiece label n v = handleMissing label v =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> (_Just . dfValue .~ n $ dropdown) <*> pure v
    dropdown = v ^? getDropdown label

dropdownUpdate :: Text -> Int -> Value -> Either Text Update
dropdownUpdate label n v = toUpdate <$> (_Right . dfValue .~ n $ dropdown)
  where
    dropdown = maybeToEither ("Could not locate dropdown " <> tshow label) $ v ^? getDropdown label

clickButton :: PathPiece ReportId -> Text -> Value -> Appian Value
clickButton = clickButton' uiUpdate

clickButton' :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> Value -> Appian Value
clickButton' f pathPiece label v = handleMissing label v =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> btn <*> pure v
    btn = v ^? getButton label

buttonUpdate :: Text -> Value -> Either Text Update
buttonUpdate label v = toUpdate <$> btn
  where
    btn = maybeToEither ("Could not locate button " <> tshow label) $ v ^? getButton label

sendText :: PathPiece TaskId -> Text -> Text -> Value -> Appian Value
sendText = sendText' taskUpdate

sendText' :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> Text -> Value -> Appian Value
sendText' f pathPiece label txt v = handleMissing label v =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> (_Just . tfValue .~ txt $ tf) <*> pure v
    tf = v ^? getTextField label

textUpdate :: Text -> Text -> Value -> Either Text Update
textUpdate label txt v = toUpdate <$> (_Right . tfValue .~ txt $ tf)
  where
    tf = maybeToEither ("Could not find TextField " <> tshow label) $ v ^? getTextField label

-- sendPicker :: (FromJSON b, ToJSON b) => (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> AppianPickerData b -> Value -> Appian Value
-- sendPicker f pathPiece label uname v = handleMissing label v =<< (sequence $ f pathPiece <$> up)
--   where
--     up = join $ mkUpdate <$> (_Just . pwValue . _JSON .~ (Just uname) $ apd) <*> pure v
--     apd = v ^? getPickerWidget label

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

dynLinksUpdate :: Text -> Int -> Value -> Either Text Update
dynLinksUpdate column idx v = toUpdate <$> maybeToEither ("Could not locate any dynamic links in column " <> tshow column) (v ^? dropping idx (getGridWidgetDynLink column))

-- gridFieldDynLinkUpdate :: (Applicative f, Contravariant f) => Text -> (Update -> f Update) -> Value -> f Value
gridFieldDynLinkUpdate :: Text -> Value -> Either Text Update
gridFieldDynLinkUpdate column v = toUpdate <$> maybeToEither ("Could not locate any dynamic links in column " <> tshow column) (v ^? getGridFieldCell . traverse . gfColumns . at column . traverse . _TextCellDynLink . _2 . traverse)

sendUpdate :: (UiConfig (SaveRequestList Update) -> Appian Value) -> Either Text (UiConfig (SaveRequestList Update)) -> Appian Value
sendUpdate f update = do
  eRes <- sendUpdate' f update
  case eRes of
    Left v -> throwM v
    Right res -> return res

sendUpdate' :: (UiConfig (SaveRequestList Update) -> Appian Value) -> Either Text (UiConfig (SaveRequestList Update)) -> Appian (Either ValidationsException Value)
sendUpdate' _ (Left msg) = throwM $ BadUpdateException msg Nothing
sendUpdate' f (Right x) = do
  eV <- tryAny $ f x
  case eV of
    Left exc -> throwM $ ServerException (exc, x)
    Right v ->
      case v ^.. cosmos . key "validations" . _Array . filtered (not . onull) . traverse . key "message" . _String of
        [] -> return $ Right v
        l -> return $ Left $ ValidationsException (l, v, x)

handleMissing :: Text -> Value -> Maybe a -> Appian a
handleMissing label v Nothing = throwM $ BadUpdateException label (Just v)
handleMissing label _ (Just component) = return component

makeSelection :: Value -> Text -> Int -> Maybe DropdownField
makeSelection v label n = v ^? hasKeyValue "label" label . _JSON . to (dfValue .~ n)

mkUpdate :: ToUpdate a => a -> Value -> Maybe (UiConfig (SaveRequestList Update))
mkUpdate component v = v ^? _JSON . to (id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)) . to (uiUpdates .~ (Just . SaveRequestList . pure . toUpdate $ component))

mkUiUpdate :: [Update] -> Value -> Either Text (UiConfig (SaveRequestList Update))
mkUiUpdate [] _ = Left "No updates to send!"
mkUiUpdate saveList v = maybeToEither "Could not create UiUpdate" $ v ^? _JSON . to (id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)) . to (uiUpdates .~ (Just $ SaveRequestList saveList))

textFieldCidUpdate :: Text -> Text -> Value -> Either Text Update
textFieldCidUpdate cid txt v = toUpdate <$> (_Right . tfValue .~ txt $ tf)
  where
    tf = maybeToEither ("Could not locate TextField with _cId " <> tshow cid) $ v ^? textFieldCid cid

textFieldCid :: (Applicative f, Contravariant f) => Text -> (TextField -> f TextField) -> Value -> f Value
textFieldCid cid = hasKeyValue "_cId" cid . _JSON

getReportId :: Text -> Value -> Appian ReportId
getReportId label v = handleMissing label v $ (getReportLink label >=> parseReportId) v

getTaskId :: Value -> Appian TaskId
getTaskId v = handleMissing "taskId" v $ v ^? key "taskId" . _String . to TaskId

landingPageLink :: (AsValue s, Plated s, Applicative f) => Text -> (Text -> f Text) -> s -> f s
landingPageLink label = deep (filtered $ has $ key "values" . key "values" . _Array . traverse . key "#v" . _String . only label) . key "link" . key "uri" . _String

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

checkboxGroupUpdate :: (Contravariant f, Plated s, AsValue s, AsJSON s, Applicative f) => Text -> [Int] -> Over (->) f s s (Either Text Update) (Either Text Update)
checkboxGroupUpdate label selection = failing (getCheckboxGroup label . to (cbgValue .~ Just selection) . to toUpdate . to Right) (to $ const $ Left $ "Could not find CheckboxField " <> tshow label)

componentUpdate :: ReifiedFold Value (Result Update) -> Value -> Either Text Update
componentUpdate fold v = update
  where
    update = case component of
      Error msg -> Left $ pack msg
      Success comp -> Right $ comp
    component = v ^. runFold fold

resultToEither :: Result a -> Either Text a
resultToEither (Error msg) = Left $ pack msg
resultToEither (Success a) = Right a

-- uiUpdateList :: ReifiedFold Value Update -> Value -> Appian (UiConfig (SaveRequestList Update))
-- uiUpdateList f v = do
--   taskId <- getTaskId v
--   let tid = PathPiece taskId
--       updates = v ^.. runFold f
--   handleMissing "UpdateList" $ mkUiUpdate (SaveRequestList updates) v

                             -- This is a horrible hack until I can figure out a better way to handle this.
sendUpdates :: Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> Appian Value
sendUpdates label f v = do
  eRes <- sendUpdates' label f v
  case eRes of
    Left v -> throwM v
    Right res -> return res

sendUpdates' :: Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT ClientM (Either ValidationsException Value)
sendUpdates' label f v = do
  taskId <- getTaskId v
  let tid = PathPiece taskId
  sendUpdates_ (taskUpdate tid) label f v

sendReportUpdates :: ReportId -> Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> Appian Value
sendReportUpdates reportId label f v = do
  eRes <- sendReportUpdates' reportId label f v
  case eRes of
    Left v -> throwM v
    Right res -> return res

sendReportUpdates' :: ReportId -> Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT ClientM (Either ValidationsException Value)
sendReportUpdates' reportId label f v = sendUpdates_ (uiUpdate rid) label f v
  where
    rid = PathPiece reportId

sendUpdates_ :: (UiConfig (SaveRequestList Update) -> Appian Value) -> Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> Appian (Either ValidationsException Value)
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
      atomically $ writeTChan logChan $ Msg $ intercalate ","
        [ toUrlPiece (1000 * utcTimeToPOSIXSeconds start)
        , tshow (diffToMS elapsed)
        , label
        , "200"
        ]
      return res
    l -> throwM $ MissingComponentException (intercalate "\n" l, v)

diffToMS :: NominalDiffTime -> Int
diffToMS elapsed = fromEnum elapsed `div` 1000000000

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither c = maybe (Left c) Right

runAppian :: Appian a -> ClientEnv -> Login -> IO (Either SomeException a)
runAppian f env creds = runAppian' f env creds >>= pure . bimap toException id

runAppian' :: Appian a -> ClientEnv -> Login -> IO (Either ServantError a)
runAppian' f env creds = bracket (runClientM login' env) (\cj -> runClientM (logout' cj) env) runF
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

