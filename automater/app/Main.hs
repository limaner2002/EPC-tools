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
import Data.Attoparsec.Text
import Data.Time (addDays)
import Scripts.FCCForm471
import Scripts.Opts
import Options.Applicative (execParser)

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

preprodClientEnv = do
  mgr <- newManager tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Https "portal-preprod.usac.org" 443 "")

testCreds = Login "app.full.right@testmail.usac.org" "Usac123$"

initialReviewer = Login "initial.reviewer@testmail.usac.org" "USACuser123$1"

test3Admin = Login "EPC.Application.Administrator" "USACuser123!"

test3Library = Login "libraryuser@testmail.usac.org" "Usac123$"

preprodMaria = Login "maria.carrillo@lausd.net" "EPCPassword123!"
preprodMonica = Login "monica.mayer@k12.sd.us" "EPCPassword123!"
preprodPalmer = Login "apalmer@moric.org" "EPCPassword123!"
preprodMeridith = Login "meredith.bickell@wyo.gov" "EPCPassword123!"
preprodStephen = Login "stephen.casavant@sl.universalservice.org" "EPCPassword123!"

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

viewRecordEntries :: Text -> Appian Value
viewRecordEntries recordName = do
  v <- recordsTab
  recordId <- handleMissing ("Record Id for " <> recordName) v $ v ^? hasKeyValue "label" recordName . key "value" . key "urlstub" . _String . to (RecordId . unpack)
  viewRecord recordId

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

-- main :: IO ()
-- main = do
--   env <- test3ClientEnv
--   res <- runAppian cancelThem env testCreds
--   -- res <- runAppian (do
--   --               v <- assignedPostCommit
--   --               mapM_ putStrLn $ v ^.. hasKeyValue "dashboard" "summary" . key "_recordRef" . _String
--   --           ) env initialReviewer
--   print res

main :: IO ()
main = join $ execParser scriptsInfo

-- main :: IO ()
-- main = do
--   env <- preprodClientEnv
--   res <- tryAny $ runAppian form471Intake env preprodPalmer
--   case res of
--     Left exc -> print exc
--     Right _ -> putStrLn "FCC Form 471 successfully created!"

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
    Nothing -> throwM $ MissingComponentException ("Cancel Button", v)
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

-- form486Intake :: Appian Value
-- form486Intake = do
--   v <- reportsTab
--   rid <- getReportId "My Landing Page" v
--   v' <- editReport (PathPiece rid)
--   form486Link <- handleMissing "FCC Form 486" v' $ v' ^? landingPageLink "FCC Form 486"
--   aid <- handleMissing "Action ID" v' $ parseActionId form486Link
--   pid <- landingPageAction $ PathPiece aid
--   v'' <- landingPageActionEx $ PathPiece pid
--   taskId <- getTaskId v''
--   let tid = PathPiece taskId
--       typed = TypedText "app.full.right@testmail.usac.org"
--       un = Identifiers [AppianUsername "app.full.right@testmail.usac.org"]
--       updates = v'' ^.. runFold (Fold (to (dropdownUpdate "Funding Year" 2) . traverse) <|> Fold (to (textUpdate "Nickname" "PerfTest") . traverse) <|> Fold (to (pickerUpdate "Main Contact Person" un) . traverse) <|> Fold (to (buttonUpdate "Continue") . traverse))
--   sendUpdate (taskUpdate tid) $ mkUiUpdate (updates) v''

-- form471Intake :: Appian Value
-- form471Intake = do
--   v <- reportsTab
--   rid <- getReportId "My Landing Page" v
--   v' <- editReport (PathPiece rid)
--   form471Link <- handleMissing "FCC Form 471" $ v' ^? landingPageLink "FCC Form 471"
--   aid <- handleMissing "Action ID" $ parseActionId form471Link
--   pid <- landingPageAction $ PathPiece aid
--   v'' <- landingPageActionEx $ PathPiece pid
--   taskId <- getTaskId v''
--   let tid = PathPiece taskId
--       updates = v'' ^.. runFold (    Fold (to (textUpdate "Please enter an application nickname here." "PerfTest") . traverse)
--                                  <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
--                                 )
--   basicInfo <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) v''
--   taskId <- getTaskId basicInfo
--   let tid = PathPiece taskId
--       updates = basicInfo ^.. runFold (    Fold (to (buttonUpdate "Yes") . traverse)
--                                        <|> Fold (to (paragraphUpdate "Enter Holiday Contact Information" "Stuff goes in here!") . traverse)
--                                        <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
--                                       )
--   categoryOfService <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) basicInfo
--   taskId <- getTaskId categoryOfService
--   let tid = PathPiece taskId
--       updates = categoryOfService ^.. runFold (    Fold (to (buttonUpdate "Category 1") . traverse)
--                                                <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
--                                               )
--   membersPage <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) categoryOfService

--   entityInformation <- selectMembers membersPage

--   taskId <- getTaskId entityInformation
--   let tid = PathPiece taskId
--       updates = entityInformation ^.. runFold (    Fold (to (buttonUpdate "Save & Continue") . traverse)
--                                         )
--   discounts <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) entityInformation

--   listOf470s <- sendUpdates (Fold (to (buttonUpdate "Save & Continue") . traverse)) discounts
--             >>= sendUpdates (Fold $ to (buttonUpdate "Add FRN") . traverse)
--             >>= sendUpdates (    Fold (to (textUpdate "Please enter a Funding Request Nickname here" "PerfTest") . traverse)
--                                                <|> Fold (to (buttonUpdate "No") . traverse)
--                                                <|> Fold (to (dropdownUpdate "Category 1 Service Types" 2) . traverse)
--                                                <|> Fold (to (buttonUpdate "Continue") . traverse)
--                                               )
--             >>= sendUpdates (Fold (to (buttonUpdate "Tariff") . traverse)
--                               <|> Fold (to (buttonUpdate "Continue") . traverse)
--                             )
--             >>= sendUpdates ( Fold (to (textUpdate "How many bids were received?" "3") . traverse)
--                                      <|> Fold (to (buttonUpdate "Yes") . traverse)
--                                    )
--             >>= sendUpdates (Fold $ to (buttonUpdate "Search") . traverse)

--   checkbox <- handleMissing "Form 470 Grid" $ listOf470s ^? getGridWidgetValue . gwVal . traverse . _1 . traverse

--   taskId <- getTaskId listOf470s
--   let tid = PathPiece taskId
--       updates = listOf470s ^.. runFold (Fold (to (buttonUpdate "Continue") . traverse))
--   spinSearch <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList $ toUpdate checkbox : updates) listOf470s

--   searchResult <- sendUpdates (    Fold (to (textUpdate "Search by SPIN" "143000661") . traverse)
--                                <|> Fold (to (buttonUpdate "Search") . traverse)
--                               ) spinSearch

--   return searchResult
--   checkbox <- handleMissing "SPIN Grid" $ searchResult ^? getGridWidgetValue . gwVal . traverse . _1 . traverse

--   taskId <- getTaskId searchResult
--   let tid = PathPiece taskId
--       updates = searchResult ^.. runFold (Fold (to (buttonUpdate "Continue") . traverse))
--   dates <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList $ toUpdate checkbox : updates) searchResult
--   startDate <- handleMissing "Start Date" $ dates ^? hasLabel "What is the service start date?" . dpwValue . appianDate . traverse

--   pricing <- sendUpdates (Fold (to (datePickerUpdate "When will the services end? " (AppianDate $ Just $ addDays 360 startDate)) . traverse)
--               <|> Fold (to (buttonUpdate "Continue") . traverse)
--               ) dates

--   narrative <- sendUpdates (Fold (to (buttonUpdate "No") . traverse)
--                <|> Fold (to (buttonUpdate "Continue") . traverse)
--               ) pricing

--   frnList <- sendUpdates (Fold (to (paragraphUpdate "Provide a brief explanation of the products and services that you are requesting, or provide any other relevant information regarding this Funding Request. You should also use this field to describe any updates to your entity data, such as revised student counts, entity relationships, etc, that you were unable to make after the close of the Administrative filing window for profile updates. These changes will be addressed during the application review process." "Some sort of narrative here.") . traverse)
--                            <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
--                          ) narrative

--   sendUpdates (Fold (to (dynLinksUpdate "FRN" 0) . traverse)) frnList
--     >>= sendUpdates (Fold (to (buttonUpdate "Add New FRN Line Item") . traverse))
--     >>= sendUpdates (Fold (to (dropdownUpdate "Function" 2) . traverse))
--     >>= sendUpdates (Fold (to (dropdownUpdate "Type of Connection" 2) . traverse)
--                     <|> Fold (to (buttonUpdate "Continue") . traverse)
--                     )
--     >>= sendUpdates (Fold (textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" "1000")
--                      <|> Fold (textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" "0")
--                      <|> Fold (textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" "3")
--                      <|> Fold (textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" "1000")
--                      <|> Fold (textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" "0")
--                      <|> Fold (textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" "1")
--                      <|> Fold (to (buttonUpdate "Save & Continue") . traverse)
--                     )
--     >>= sendUpdates (Fold (to (buttonUpdate "Select an Entity by Consortium Members") . traverse))
--     >>= sendUpdates (Fold (to (dropdownUpdate "Search for Consortium Member by Entity Type" 2) . traverse))
--     >>= sendUpdates (Fold (getGridWidgetValue . gwVal . traverse . _1 . traverse . to toUpdate)
--                      <|> Fold (to (buttonUpdate "Add") . traverse)
--                     )
--     >>= sendUpdates (Fold (to (buttonUpdate "Save & Continue") . traverse))
--     >>= sendUpdates (Fold (to (buttonUpdate "Continue") . traverse))
--     -- End new line item
--     >>= sendUpdates (Fold (to (buttonUpdate "Continue") . traverse))
--     >>= sendUpdates (Fold (to (buttonUpdate "Review FCC Form 471") . traverse))

-- textFieldCidUpdate :: (Applicative f, Contravariant f) => Text -> Text -> (Update -> f Update) -> Value -> f Value
-- textFieldCidUpdate cid txt = textFieldCid cid . to (tfValue .~ txt) . to toUpdate

-- textFieldCid :: (Applicative f, Contravariant f) => Text -> (TextField -> f TextField) -> Value -> f Value
-- textFieldCid cid = hasKeyValue "_cId" cid . _JSON

-- selectMembers :: Value -> Appian Value
-- selectMembers v = do
--   (v', checkboxes) <- validMemberCheckboxes v
--   taskId <- getTaskId v'
--   let tid = PathPiece taskId
--       updates = fmap toUpdate checkboxes
--   checked <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) v'

--   taskId <- getTaskId checked
--   let tid = PathPiece taskId
--       updates = checked ^.. runFold ( Fold (to (buttonUpdate "Add") . traverse))
--   added <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) checked

--   taskId <- getTaskId added
--   let tid = PathPiece taskId
--       updates = added ^.. runFold ( Fold (to (buttonUpdate "Save & Continue") . traverse))
--   sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) added

-- validMemberCheckboxes :: Value -> Appian (Value, [CheckboxGroup])
-- validMemberCheckboxes v = do
--   let refs = v ^.. taking 1 (hasType "GridWidget") . getGridWidgetRecordRefs "BEN Name" . to (PathPiece . RecordRef)
--   discountRates <- mapM (\r -> (,) <$> pure r <*> viewDiscountRates r) refs
--   taskId <- getTaskId v
--   let validRefs = discountRates ^.. traverse . filtered (has $ _2 . to insufficientDiscountRate . only False) . _1
--       tid = PathPiece taskId
--       updates = v ^.. to (buttonUpdate "No") . traverse
--   v' <- sendUpdate (taskUpdate tid) $ mkUiUpdate (SaveRequestList updates) v
--   let checkBoxes = v' ^.. taking 1 getGridWidgetValue . gwVal . traverse . filtered (has $ _2 . at "BEN Name" . traverse . deep (key "_recordRef") . _String . to (PathPiece . RecordRef) . filtered (flip elem validRefs)) . _1 . traverse
--   return (v', checkBoxes)

-- viewDiscountRates :: PathPiece RecordRef -> Appian Value
-- viewDiscountRates rid = do
--   v <- viewRecordDashboard rid (PathPiece $ Dashboard "summary")
--   dashboard <- handleMissing "Discount Rate" $ v ^? getRecordDashboard "Discount Rate"
--   viewRecordDashboard rid $ PathPiece dashboard

-- insufficientDiscountRate :: Value -> Bool
-- insufficientDiscountRate v = any (checkResult . parseResult) msgs
--   where
--     msgs = v ^.. hasKeyValue "name" "x-embedded-summary" . key "children" . plate . _String . _JSON . asValue . cosmos . key "validations" . plate . key "message" . _String
--     parseResult = parseOnly (manyTill anyChar (string "not sufficient") *> manyTill anyChar (string "Discount Rate") *> pure True)
--     checkResult (Left _) = False
--     checkResult (Right b) = b

-- getReportId :: Text -> Value -> Appian ReportId
-- getReportId label = handleMissing label . (getReportLink label >=> parseReportId)

-- getTaskId :: Value -> Appian TaskId
-- getTaskId v = handleMissing "taskId" $ v ^? key "taskId" . _String . to TaskId

-- landingPageLink :: (AsValue s, Plated s, Applicative f) => Text -> (Text -> f Text) -> s -> f s
-- landingPageLink label = deep (filtered $ has $ key "values" . key "values" . _Array . traverse . key "#v" . _String . only label) . key "link" . key "uri" . _String

