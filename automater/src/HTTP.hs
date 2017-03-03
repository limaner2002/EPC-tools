{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTP where

import ClassyPrelude

import MachineUtils hiding (filter)
import qualified MachineUtils as MU
import ParseCSV
import Data.Aeson (json, Value, ToJSON, FromJSON, encode, eitherDecode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Resource
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Simple (setRequestMethod, addRequestHeader, setRequestHeaders)

import Data.Time
import JSONTreeLA
import Control.Arrow.ListArrow

import CreateRequest
import Parsers

parseUrlThrow' :: MonadThrow m => Manager -> String -> m (Request, Manager)
parseUrlThrow' mgr url = do
  req <- parseUrlThrow url
  return (req, mgr)

nowInsert :: MonadIO m => Request -> CookieJar -> m (Request, CookieJar)
nowInsert req cj = do
  now <- liftIO $ getCurrentTime
  return $ insertCookiesIntoRequest req cj now

getCookies :: MonadResource m => ProcessA (Kleisli m) (Event (Request, Manager)) (Event CookieJar)
getCookies = makeRequest >>> evMap (responseCookieJar . snd)

getCsrfToken :: ArrowApply a => ProcessA a (Event CookieJar) (Event Cookie)
getCsrfToken = evMap destroyCookieJar >>> evMap (\l -> filter (\c -> cookie_name c == "__appianCsrfToken") l) >>> blockingSource'

csrfTokenBSPair :: Arrow a => a Cookie Header
csrfTokenBSPair = arr (const $ CI.mk "X-APPIAN-CSRF-TOKEN") &&& arr (cookie_value)

csrfTokenHeader :: ArrowApply a => ProcessA a (Event (Response body)) (Event Header)
csrfTokenHeader = evMap responseCookieJar >>> getCsrfToken >>> anytime csrfTokenBSPair

insertHeader :: Arrow a => a (Header, Request) Request
insertHeader = arr (uncurry insertHeader_)
  where
    insertHeader_ pair req = req {requestHeaders = pair : requestHeaders req}

type RsrcResp = (ReleaseKey, Response BodyReader)

-- reqManager' :: MonadResource m => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event Request, Event (Response BodyReader)) (Event (ReleaseKey, Response BodyReader), Event (Response BodyReader))
-- reqManager' mgr initReq loginReq = switch before after
--   where
--     before = proc (req, _) -> do
--       resp <- makeRequest >>> evMap snd -< (initReq, mgr) <$ req
--       csrfToken <- csrfTokenHeader -< resp

--       req' <- id *** evMap responseCookieJar >>> mergeEvents >>> machine (uncurry nowInsert) >>> evMap fst >>> evMap (addRequestHeaders baseHeaders) -< (loginReq <$ resp, resp)
--       req'' <- mergeEvents >>> anytime insertHeader -< (csrfToken, req')
--       req''' <- mergeEvents >>> evMap (uncurry appendQueryString) -< (csrfToken, req'')

--       respWithKey <- anytime (passthroughK $ print . fst) >>> makeRequest -< fmap (\req -> (req, mgr)) req'''
--       resp' <- evMap snd -< respWithKey

--       returnA -< ((respWithKey, resp'), resp')
--     after initResp = proc (reqEvt, respFB) -> do
--       oldResp <- anytime (passthroughK $ const $ putStrLn "Feedback fired!") >>> hold initResp -< respFB

--       req <- setReq mgr -< (reqEvt, oldResp <$ reqEvt)
--       respWithKey <- anytime (passthroughK $ print . fst) >>> makeRequest -< fmap (\req -> (req, mgr)) req

--       oldResp' <- evMap snd -< respWithKey
--       returnA -< (respWithKey, oldResp')

-- reqManager :: (FromJSON a, ToJSON a, MonadResource m, Show a) => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event (LA (Either SomeException Value) (Action a)), Event RsrcResp) (Event RsrcResp, Event RsrcResp)
-- reqManager mgr initReq loginReq = switch before after
reqManager :: (FromJSON a, ToJSON a, MonadResource m, Show a) => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event (LA Value (Action a))) (Event RsrcResp)
reqManager mgr initReq loginReq = switch before (reqManager_ mgr)
  where
    before = proc act -> do
      resp <- makeRequest -< (initReq, mgr) <$ act
      csrfToken <- evMap snd >>> csrfTokenHeader -< resp

      req' <- id *** evMap (responseCookieJar . snd) >>> mergeEvents >>> machine (uncurry nowInsert) >>> evMap fst >>> evMap (addRequestHeaders baseHeaders) -< (loginReq <$ resp, resp)
      req'' <- mergeEvents >>> anytime insertHeader -< (csrfToken, req')
      req''' <- mergeEvents >>> evMap (uncurry appendQueryString) -< (csrfToken, req'')

      respWithKey <- makeRequest -< fmap (\req -> (req, mgr)) req'''

      returnA -< (respWithKey, respWithKey)

reqManager_ :: (FromJSON a, ToJSON a, MonadResource m, Show a) => Manager -> RsrcResp -> ProcessA (Kleisli m) (Event (LA Value (Action a))) (Event RsrcResp)
reqManager_ mgr oldResp = switch before after
  where
    before = proc actEvt -> do
      req <- updateRequest -< fmap (\act -> (act, oldResp)) actEvt
      req' <- id *** evMap snd >>> setReq mgr -< (req, oldResp <$ req)
      respWithKey <- makeRequest >>> anytime (passthroughK $ const $ putStrLn "Made request") -< fmap (\r -> (r, mgr)) req'

      returnA -< (respWithKey, respWithKey)
    after old = reqManager_ mgr old
      

setReq :: MonadResource m => Manager -> ProcessA (Kleisli m) (Event Request, Event (Response BodyReader)) (Event Request)
setReq mgr = proc (reqEvt, oldResp) -> do
  csrfToken <- csrfTokenHeader -< oldResp
  req' <- mergeEventsL >>> evMap (id *** arr responseCookieJar) >>> machine (uncurry nowInsert) >>> evMap fst >>> evMap (addRequestHeaders baseHeaders) -< (reqEvt, oldResp)
  req'' <- mergeEventsL >>> anytime insertHeader -< (csrfToken, req')
  returnA -< req''


login :: MonadResource m => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event a, Event RsrcResp) ((Event RsrcResp, Event RsrcResp), Event RsrcResp)
login mgr initReq loginReq = proc (a, _) -> do
  resp <- makeRequest >>> evMap snd -< (initReq, mgr) <$ a
  csrfToken <- csrfTokenHeader -< resp

  req' <- id *** evMap responseCookieJar >>> mergeEvents >>> machine (uncurry nowInsert) >>> evMap fst >>> evMap (addRequestHeaders baseHeaders) -< (loginReq <$ resp, resp)
  req'' <- mergeEvents >>> anytime insertHeader -< (csrfToken, req')
  req''' <- mergeEvents >>> evMap (uncurry appendQueryString) -< (csrfToken, req'')

  respWithKey <- makeRequest -< fmap (\req -> (req, mgr)) req'''

  returnA -< ((respWithKey, respWithKey), respWithKey)

updateRequest :: (ToJSON a, FromJSON a, MonadResource m, Show a) => ProcessA (Kleisli m) (Event (LA Value (Action a), RsrcResp)) (Event Request)
updateRequest =
  evMap fst &&& (evMap snd >>> sourceHttp_ >>> evMap snd >>> machineParser' appianResponseParser)
  >>> mergeEventsL
  >>> evMap (\(act, resp) -> runLA act resp)
  >>> blockingSource'
  >>> machine mkUpdateRequest
  >>> anytime (passthroughK $ \x -> putStrLn $ "Creating new request: " <> tshow x)

testIt :: IO ()
testIt = do
  mgr <- newManager tlsManagerSettings
  siteReq <- parseRequest "https://portal-test.usac.org"
  loginReq <- authReq baseUrl "app1_lb1_full1@mailinator.com" "USACuser123$" mempty

  let loadRep = mkHRef "/suite/rest/a/uicontainer/latest/YB7oUg/view" "GET" :: Action Text
      logoutReq = mkHRef "/suite/logout" "GET"  :: Action Text
      caseReq = mkHRef "https://portal-test.usac.org/suite/api/tempo/open-a-case/action/ksBnWUR1XjkLMmEri-oDn0gL2Fmrr_v3Fcs3gqyQpKrFxfzkjtesbm7cYTL9ZE8wYQ1dhVXb6DJV6r4NQGEX3geSw_O-io80YMwqMk" "GET" :: Action Text
      taskReq = mkHRef "https://portal-test.usac.org/suite/rest/a/model/latest/4064" "POST"

  runRMachine_ (reqManager mgr siteReq loginReq >>> evMap snd >>> evMap responseStatus >>> machine print)
    [ arr (const loadRep)
    -- , openCaseLink
    -- , arr (const taskReq)
    -- , text "Nickname" "PerfTest"
    , arr (const loadRep)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    , arr (const logoutReq)
    ]
  -- runRMachine_ (machine mkUpdateRequest >>> loop (reqManager' mgr siteReq loginReq) >>> evMap snd >>> evMap responseStatus >>> machine print) [loadRep, caseReq, logoutReq]
  where
    asEither :: Either SomeException a -> Either SomeException a
    asEither = id

authReq :: MonadThrow m => String -> Text -> Text -> [(ByteString, Maybe ByteString)] -> m Request
authReq baseUrl un pw params = setRequestMethod "POST"
  <$> addRequestHeader "Content-Type" "application/x-www-form-urlencoded"
  <$> setQueryString (params <> authParams (encodeUtf8 un) (encodeUtf8 pw))
  <$> parseUrlThrow (baseUrl <> "/suite/auth?appian_environment=tempo")

baseUrl :: IsString t => t
baseUrl = "https://portal-test.usac.org"

baseHeaders :: [Header]
baseHeaders = [ ("Accept-Language", "en-US,en;q=0.8")
              , ("Upgrade-Insecure-Requests", "1")
              , ("Accept-Encoding", "gzip, deflate, sdch, br")
              , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Safari/602.1.50")
              , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
              , ("Connection", "keep-alive")
              ]

authParams :: ByteString -> ByteString -> [(ByteString, Maybe ByteString)]
authParams un pw = [ ("un", Just un)
           , ("pw", Just pw)
           , ("spring_security_remember_me", Just "on")
           ]

appendQueryString :: Header -> Request -> Request
appendQueryString (n, v) req = req { queryString = qs <> "&" <> newS }
  where
    qs = queryString req
    newS = CI.original n <> "=" <> v

addRequestHeaders :: [Header] -> Request -> Request
addRequestHeaders heads req = req { requestHeaders = requestHeaders req <> heads }

mkRequest :: MonadThrow m => String -> m Request
mkRequest path = parseRequest (baseUrl <> path)

mkUpdateRequest :: (ToJSON a, MonadThrow m) => Action a -> m Request
mkUpdateRequest (HRef url method)
  | isPrefixOf baseUrl url == False = do
      req <- parseRequest . unpack $ baseUrl <> url
      return $ req {method = encodeUtf8 method}
  | otherwise = do
      req <- parseRequest $ unpack url
      return $ req {method = encodeUtf8 method}
mkUpdateRequest upd@(Update {taskId = taskId}) = do
  req <- parseRequest . unpack $ "/suite/rest/a/task/latest/" <> show taskId <> "/form"
  return $ req { requestBody = RequestBodyLBS (encode upd)
               , method = "POST"
               }

mkHRef :: Text -> Text -> Action a
mkHRef url method
  | isPrefixOf "http://" url || isPrefixOf "https://" url = HRef url method
  | otherwise = HRef (baseUrl <> url) method

-- Merge two events into one and fire when the left event fires
mergeEventsL :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEventsL = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  mB <- evMap Just >>> hold Nothing -< evtB
  case f mA mB of
    Nothing -> returnA -< noEvent
    Just (a, b) -> returnA -< (a, b) <$ evtA
  where
    f mA mB = (,) <$> mA <*> mB

mergeAccumL :: (ArrowApply a, Monoid c, Semigroup c) => ProcessA a (Event b, Event c) (Event (b, c))
mergeAccumL = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  acc <- evMap (<>) >>> accum mempty -< evtB
  case mA of
    Nothing -> returnA -< noEvent
    Just a -> returnA -< (a, acc) <$ evtA

(>>|) :: ArrowApply cat => ProcessA cat (Event a) (Event d) -> ProcessA cat (Event b) (Event d) -> ProcessA cat (Event (Either a b)) (Event d)
(>>|) catA catB = proc input -> do
  mV <- evMap Just >>> hold Nothing -< input
  case mV of
    Nothing -> returnA -< noEvent
    Just (Left v) -> catA >>> returnA -< v <$ input
    Just (Right v) -> catB >>> returnA -< v <$ input

infixr 5 >>|

