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
type UIUpdate update = "suite" :> "rest" :> "a" :> "uicontainer" :> "latest" :> PathPiece ReportId :> "view" :> ReqBody '[AppianTV] (UiConfig update) :> AppianCsrfToken :> Post '[AppianTVUI] Value
type RelatedActionEx = Header "X-Appian-Features" Text :> Url :> EmptyAppianTV :> AppianCsrfToken :> Post '[AppianTVUI] Value
type AvailableActions = "suite" :> "api" :> "tempo" :> "open-a-case" :> "available-actions" :> Header "X-Appian-Features" Text :> AppianCsrfToken :> Get '[JSON] Value
type LandingPageAction = "suite" :> "api" :> "tempo" :> "open-a-case" :> "action" :> PathPiece ActionId :> AppianCsrfToken :> Get '[JSON] ProcessModelId
type LandingPageActionEx = "suite" :> "rest" :> "a" :> "model" :> "latest" :> PathPiece ProcessModelId :> EmptyAppianTV :> Header "X-Appian-Features" Text :> Header "X-Appian-Ui-State" Text :> Header "X-Client-Mode" Text :> AppianCsrfToken :> Post '[AppianTVUI] Value

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
--   :<|> RecordActions
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
--  :<|> recordActions_
  :<|> relatedActionEx_
  :<|> actionsTab_
  :<|> landingPageAction_
  :<|> landingPageActionEx_
  = client api

recordsTab = Appian recordsTab_
viewRecordDashboard e dashboard = liftIO (putStrLn $ "View Record Dashboard" <> tshow dashboard) >> Appian (viewRecordEntry_ e dashboard)
viewRecord r = Appian (viewRecord_ r)
viewRecordEntry e = Appian (viewRecordEntry_ e (PathPiece $ Dashboard "summary"))
tasksTab mUtcTime = Appian (tasksTab_ mUtcTime)
taskAttributes tid = Appian (taskAttributes_ tid)
acceptTask tid = Appian (acceptTask_ tid)
taskStatus tid = Appian (taskStatus_ (Just ("ceebc" :: Text)) (Just "stateful") (Just "TEMPO") tid ("accepted" :: Text))
reportsTab = Appian reportsTab_
editReport rid = Appian (editReport_ rid)
recordActions rid = Appian (viewRecordEntry_ rid (PathPiece $ Dashboard "actions"))
relatedActionEx url = Appian (relatedActionEx_ (Just "ceebc") url)
actionsTab = Appian (actionsTab_ (Just "e4bc"))
landingPageAction aid = Appian (landingPageAction_ aid)
landingPageActionEx pid = Appian (landingPageActionEx_ pid (Just "ceebc") (Just "stateful") (Just "TEMPO"))

taskUpdate :: ToJSON update => PathPiece TaskId -> UiConfig update -> Appian Value
taskUpdate tid ui = Appian (taskUpdate_ tid ui)
  where
    taskUpdate_ = client (Proxy :: ToJSON update => Proxy (TaskUpdate update)) (Just ("ceebc" :: Text)) (Just ("en-US,en;q=0.8" :: Text)) (Just "stateful") (Just "TEMPO")

uiUpdate :: ToJSON update => PathPiece ReportId -> UiConfig update -> Appian Value
uiUpdate rid ui = Appian (uiUpdate_ rid ui)
  where
    uiUpdate_ = client (Proxy :: ToJSON update => Proxy (UIUpdate update))

getCurrentTaskIds :: Appian [Text]
getCurrentTaskIds = do
  v <- tasksTab Nothing
  return $ v ^.. getTaskIds

getTaskIds :: (Contravariant f, AsValue s, Plated s, Applicative f) => (Text -> f Text) -> s -> f s
getTaskIds = deep (key "entries") . _Array . traverse . key "id" . _String . to (stripPrefix "t-") . _Just

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

newtype MissingComponentException = MissingComponentException Text

instance Show MissingComponentException where
  show (MissingComponentException name) = "MissingComponentException: Could not find component " <> show name

instance Exception MissingComponentException

newtype ValidationsException = ValidationsException { _validationsExc :: ([Text], Value, UiConfig (SaveRequestList Update)) }

-- makeLenses ''ValidationsException

instance Show ValidationsException where
  show (ValidationsException validations) = "ValidationsException: \n" <> intercalate "\n" (fmap unpack $ validations ^. _1)

instance Exception ValidationsException

newtype ServerException = ServerException { _serverExc :: (SomeException, UiConfig (SaveRequestList Update)) }

instance Show ServerException where
  show (ServerException exc) = "ServerException: " <> show (exc ^. _1)

instance Exception ServerException

sendSelection :: PathPiece ReportId -> Text -> Int -> Value -> Appian Value
sendSelection = sendSelection' uiUpdate

sendSelection' :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> Int -> Value -> Appian Value
sendSelection' f pathPiece label n v = handleMissing label =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> (_Just . dfValue .~ n $ dropdown) <*> pure v
    dropdown = v ^? getDropdown label

dropdownUpdate :: Text -> Int -> Value -> Maybe Update
dropdownUpdate label n v = toUpdate <$> (_Just . dfValue .~ n $ dropdown)
  where
    dropdown = v ^? getDropdown label

clickButton :: PathPiece ReportId -> Text -> Value -> Appian Value
clickButton = clickButton' uiUpdate

clickButton' :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> Value -> Appian Value
clickButton' f pathPiece label v = handleMissing label =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> btn <*> pure v
    btn = v ^? getButton label

buttonUpdate :: Text -> Value -> Maybe Update
buttonUpdate label v = toUpdate <$> btn
  where
    btn = v ^? getButton label

sendText :: PathPiece TaskId -> Text -> Text -> Value -> Appian Value
sendText = sendText' taskUpdate

sendText' :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> Text -> Value -> Appian Value
sendText' f pathPiece label txt v = handleMissing label =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> (_Just . tfValue .~ txt $ tf) <*> pure v
    tf = v ^? getTextField label

textUpdate :: Text -> Text -> Value -> Maybe Update
textUpdate label txt v = toUpdate <$> (_Just . tfValue .~ txt $ tf)
  where
    tf = v ^? getTextField label

sendPicker :: (PathPiece a -> UiConfig (SaveRequestList Update) -> Appian Value) -> PathPiece a -> Text -> AppianPickerData -> Value -> Appian Value
sendPicker f pathPiece label uname v = handleMissing label =<< (sequence $ f pathPiece <$> up)
  where
    up = join $ mkUpdate <$> (_Just . pwValue . _JSON .~ uname $ apd) <*> pure v
    apd = v ^? getPickerWidget label

pickerUpdate :: Text -> AppianPickerData -> Value -> Maybe Update
pickerUpdate label uname v = toUpdate <$> (_Just . pwValue .~ toJSON uname $ apd)
  where
    apd = v ^? getPickerWidget label

paragraphUpdate :: Text -> Text -> Value -> Maybe Update
paragraphUpdate label txt v = toUpdate <$> (_Just . pgfValue .~ txt $ pgf)
  where
    pgf = v ^? getParagraphField label

datePickerUpdate :: Text -> AppianDate -> Value -> Maybe Update
datePickerUpdate label date v = toUpdate <$> (_Just . dpwValue .~ date $ dpw)
  where
    dpw = v ^? getDatePicker label

dynLinksUpdate :: Text -> Int -> Value -> Maybe Update
dynLinksUpdate column idx v = toUpdate <$> v ^? dropping idx (getGridWidgetDynLink column)

sendUpdate :: (UiConfig (SaveRequestList Update) -> Appian Value) -> Maybe (UiConfig (SaveRequestList Update)) -> Appian Value
sendUpdate _ Nothing = throwM $ MissingComponentException "Cannot create update"
sendUpdate f (Just x) = do
  eV <- tryAny $ f x
  case eV of
    Left exc -> throwM $ ServerException (exc, x)
    Right v ->
      case v ^.. cosmos . key "validations" . _Array . filtered (not . onull) . traverse . key "message" . _String of
        [] -> return v
        l -> throwM $ ValidationsException (l, v, x)

handleMissing :: Text -> Maybe a -> Appian a
handleMissing label Nothing = throwM $ MissingComponentException label
handleMissing label (Just component) = return component

makeSelection :: Value -> Text -> Int -> Maybe DropdownField
makeSelection v label n = v ^? hasKeyValue "label" label . _JSON . to (dfValue .~ n)

mkUpdate :: ToUpdate a => a -> Value -> Maybe (UiConfig (SaveRequestList Update))
mkUpdate component v = v ^? _JSON . to (id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)) . to (uiUpdates .~ (Just . SaveRequestList . pure . toUpdate $ component))

mkUiUpdate :: SaveRequestList Update -> Value -> Maybe (UiConfig (SaveRequestList Update))
mkUiUpdate saveList v = v ^? _JSON . to (id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)) . to (uiUpdates .~ Just saveList)
