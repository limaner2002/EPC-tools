{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Appian.Client


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

initialReviewer = Login "initial.reviewer@testmail.usac.org" "USACuser123$1"

test3Admin = Login "EPC.Application.Administrator" "USACuser123!"

test3Library = Login "libraryuser@testmail.usac.org" "Usac123$"

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

getEditNotesIds :: Maybe UTCTime -> Appian ([(Text, Text)], Maybe UTCTime)
getEditNotesIds mTime = do
  v <- tasksTab mTime
  --let next = v ^? deep (filtered $ has $ key "rel" . _String . only "next") . key "href" . _String . to (lookup "b" . toQueryMap . parseQueryHack) . traverse . to readMay . traverse
  let next = v ^? deep (filtered $ has $ key "rel" . _String . only "next") . key "href" . _String . to (lookup "b" . toQueryMap . parseQueryHack) . traverse . to tshow . _JSON
  return $ (v ^.. deep (filtered $ has $ key "content") . runFold ((,) <$> Fold (key "content" . _String) <*> Fold (key "id" . _String)), next)

parseQueryHack :: Text -> [[[Text]]]
parseQueryHack = (fmap . fmap) (splitSeq "=") . fmap (splitSeq "&") . drop 1 . splitElem '?'

toQueryMap :: [[[Text]]] -> Map Text Text
toQueryMap l = mapFromList $ l ^.. traverse . traverse . filtered (\l' -> length l' == 2) . to mkPair
  where
    mkPair [x, y] = (x, y)

main :: IO ()
main = do
  env <- test3ClientEnv
  res <- runAppian cancelThem env testCreds
  -- res <- runAppian (do
  --               v <- assignedPostCommit
  --               mapM_ putStrLn $ v ^.. hasKeyValue "dashboard" "summary" . key "_recordRef" . _String
  --           ) env initialReviewer
  print res

assignedPostCommit :: Appian Value
assignedPostCommit =
      editReport rid
  >>= sendSelection rid "Review Type" 2
  >>= sendSelection rid "Reviewer Type" 2
  >>= sendSelection rid "Funding Year" 2
  >>= clickButton rid "Apply Filters"
  where
    rid = PathPiece $ ReportId "yMOz4g"

executeRelatedAction :: Text -> PathPiece RecordRef -> Appian (Maybe Value)
executeRelatedAction action rid = do
  v <- recordActions rid
  let url = v ^? hasKeyValue "title" action . deep (hasKeyValue "title" "Execute related action") . key "href" . _String . to (stripPrefix "https://portal-test3.appiancloud.com") . traverse
  sequence $ relatedActionEx <$> url

cancelTask :: TaskId -> Appian Value
cancelTask tid = do
  v <- taskStatus $ PathPiece tid
  let mReq = (_Just . uiUpdates .~ (SaveRequestList . pure . toUpdate <$> v ^? getCloseButton)) $ v ^? _JSON . to asUiConfig
      asUiConfig = id :: UiConfig (SaveRequestList Update) -> UiConfig (SaveRequestList Update)
  case mReq of
    Nothing -> trace "Throwing an error right now!" (throwM $ MissingComponentException "Cancel Button")
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

form486Intake :: Appian Value
form486Intake = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport (PathPiece rid)
  form486Link <- handleMissing "FCC Form 486" $ v' ^? landingPageLink "FCC Form 486"
  aid <- handleMissing "Action ID" $ parseActionId form486Link
  pid <- landingPageAction $ PathPiece aid
  v'' <- landingPageActionEx $ PathPiece pid
  taskId <- getTaskId v''
  let tid = PathPiece taskId
      typed = TypedText "app.full.right@testmail.usac.org"
      un = Identifiers [AppianUsername "app.full.right@testmail.usac.org"]
      updates = v'' ^.. runFold (Fold (to (dropdownUpdate "Funding Year" 2) . traverse) <|> Fold (to (textUpdate "Nickname" "PerfTest") . traverse) <|> Fold (to (pickerUpdate "Main Contact Person" un) . traverse) <|> Fold (to (buttonUpdate "Continue") . traverse))
  sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) v''

form471Intake :: Appian Value
form471Intake = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport (PathPiece rid)
  form471Link <- handleMissing "FCC Form 471" $ v' ^? landingPageLink "FCC Form 471"
  aid <- handleMissing "Action ID" $ parseActionId form471Link
  pid <- landingPageAction $ PathPiece aid
  v'' <- landingPageActionEx $ PathPiece pid
  taskId <- getTaskId v''
  let tid = PathPiece taskId
      updates = v'' ^.. runFold (    Fold (to (textUpdate "Please enter an application nickname here." "PerfTest") . traverse)
                                 <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
                                )
  basicInfo <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) v''
  taskId <- getTaskId basicInfo
  let tid = PathPiece taskId
      updates = basicInfo ^.. runFold (    Fold (to (buttonUpdate "Yes") . traverse)
                                       <|> Fold (to (paragraphUpdate "Enter Holiday Contact Information" "Stuff goes in here!") . traverse)
                                       <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
                                      )
  categoryOfService <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) basicInfo
  taskId <- getTaskId categoryOfService
  let tid = PathPiece taskId
      updates = categoryOfService ^.. runFold (    Fold (to (buttonUpdate "Category 1") . traverse)
                                               <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
                                              )
  membersPage <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) categoryOfService
  taskId <- getTaskId membersPage
  let tid = PathPiece taskId
      updates = membersPage ^.. runFold (    Fold (to (buttonUpdate "Save & Continue") . traverse)
                                        )
  entityInformation <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) membersPage
  taskId <- getTaskId entityInformation
  let tid = PathPiece taskId
      updates = entityInformation ^.. runFold (    Fold (to (buttonUpdate "Save & Continue") . traverse)
                                        )
  discounts <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) entityInformation
  taskId <- getTaskId discounts
  let tid = PathPiece taskId
      updates = discounts ^.. to (buttonUpdate "Show Additional Information") . traverse
  sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) discounts

getReportId :: Text -> Value -> Appian ReportId
getReportId label = handleMissing label . (getReportLink label >=> parseReportId)

getTaskId :: Value -> Appian TaskId
getTaskId v = handleMissing "taskId" $ v ^? key "taskId" . _String . to TaskId

landingPageLink :: (AsValue s, Plated s, Applicative f) => Text -> (Text -> f Text) -> s -> f s
landingPageLink label = deep (filtered $ has $ key "values" . key "values" . _Array . traverse . key "#v" . _String . only label) . key "link" . key "uri" . _String
