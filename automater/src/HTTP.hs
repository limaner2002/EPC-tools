{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module HTTP where

import ClassyPrelude

import MachineUtils hiding (filter, mergeEvents)
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
getCsrfToken = evMap destroyCookieJar >>> evMap (\l -> filter (\c -> cookie_name c == "__appianCsrfToken") l) >>> MU.fork

csrfTokenBSPair :: Arrow a => a Cookie Header
csrfTokenBSPair = arr (const $ CI.mk "X-APPIAN-CSRF-TOKEN") &&& arr (cookie_value)

csrfTokenHeader :: ArrowApply a => ProcessA a (Event (Response body)) (Event Header)
csrfTokenHeader = evMap responseCookieJar >>> getCsrfToken >>> anytime csrfTokenBSPair

insertHeader :: Arrow a => a (Header, Request) Request
insertHeader = arr (uncurry insertHeader_)
  where
    insertHeader_ pair req = req {requestHeaders = pair : requestHeaders req}

type RsrcResp = (ReleaseKey, Response BodyReader)

reqManager :: (FromJSON a, ToJSON a, MonadResource m) => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event (LA Value (Action a))) (Event RsrcResp)
reqManager mgr initReq loginReq = switch (login mgr initReq loginReq) after
  where
    after initResp = proc actEvt -> do
      rec
        req <- updateRequest -< fmap (\act -> (act, oldResp)) actEvt
        req' <- setReq -< fmap (\r -> (r, oldResp)) req
        respWithKey <- makeRequest -< fmap (\r -> (r, mgr)) req'
        oldResp <- dHold initResp -< respWithKey
      returnA -< respWithKey

cookieManager :: MonadResource m => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event Request) (Event RsrcResp)
cookieManager mgr initReq loginReq = switch (login mgr initReq loginReq) after
  where
    after initResp = proc reqEvt -> do
      rec
        req <- anytime (passthroughK (print . fst)) >>> setReq -< fmap (\r -> (r, oldResp)) reqEvt
        respWithKey <- makeRequest -< fmap (\r -> (r, mgr)) req
        oldResp <- dHold initResp -< respWithKey
      returnA -< respWithKey

setReq :: MonadResource m => ProcessA (Kleisli m) (Event (Request, RsrcResp)) (Event Request)
setReq = proc input -> do
  csrfToken <- evMap (snd . snd) >>> csrfTokenHeader -< input
  req' <- evMap (id *** arr (responseCookieJar . snd)) >>> machine (uncurry nowInsert) >>> evMap fst >>> evMap (addRequestHeaders baseHeaders) -< input
  req'' <- mergeEventsR >>> anytime insertHeader -< (csrfToken, req')
  returnA -< req''


login :: MonadResource m => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event a) (Event RsrcResp, Event RsrcResp)
login mgr initReq loginReq = proc a -> do
  resp <- makeRequest >>> evMap snd -< (initReq, mgr) <$ a
  csrfToken <- csrfTokenHeader -< resp

  req' <- id *** evMap responseCookieJar >>> mergeEventsL >>> machine (uncurry nowInsert) >>> evMap fst >>> evMap (addRequestHeaders baseHeaders) -< (loginReq <$ resp, resp)
  req'' <- mergeEventsR >>> anytime insertHeader -< (csrfToken, req')
  req''' <- mergeEventsR >>> evMap (uncurry appendQueryString) -< (csrfToken, req'')

  respWithKey <- makeRequest -< fmap (\req -> (req, mgr)) req'''

  returnA -< (respWithKey, respWithKey)

updateRequest :: (ToJSON a, FromJSON a, MonadResource m) => ProcessA (Kleisli m) (Event (LA Value (Action a), RsrcResp)) (Event Request)
updateRequest =
  evMap fst &&& (evMap snd >>> sourceHttp_ >>> evMap snd >>> machineParser' appianResponseParser)
  >>> mergeEventsR
  >>> evMap (\(act, resp) -> take 1 $ runLA act resp)
  >>> MU.fork
  >>> machine mkUpdateRequest
  >>> anytime (passthroughK $ \x -> putStrLn $ "Creating new request: " <> tshow x)

testIt :: IO ()
testIt = do
  mgr <- newManager tlsManagerSettings
  siteReq <- parseRequest "https://portal-test.usac.org"
  loginReq <- authReq baseUrl "app1_lb1_full1@mailinator.com" "USACuser123$" mempty

  -- loadRepReq <- parseRequest "https://portal-test.usac.org/suite/rest/a/uicontainer/latest/YB7oUg/view"
  -- logoutReq <- parseRequest "https://portal-test.usac.org/suite/logout"

  -- runRMachine_ (cookieManager mgr siteReq loginReq >>> evMap snd >>> evMap responseStatus >>> machine print)
  --   [ loadRepReq
  --   , logoutReq
  --   ]

  let loadRep = mkHRef "/suite/rest/a/uicontainer/latest/YB7oUg/view" "GET" :: Action Text
      logoutReq = mkHRef "/suite/logout" "GET"  :: Action Text
      caseReq = mkHRef "https://portal-test.usac.org/suite/api/tempo/open-a-case/action/ksBnWUR1XjkLMmEri-oDn0gL2Fmrr_v3Fcs3gqyQpKrFxfzkjtesbm7cYTL9ZE8wYQ1dhVXb6DJV6r4NQGEX3geSw_O-io80YMwqMk" "GET" :: Action Text
      taskReq = mkHRef "https://portal-test.usac.org/suite/rest/a/model/latest/4064" "POST"

  runRMachine_ (reqManager mgr siteReq loginReq >>> evMap (responseStatus . snd) >>> machine print) -- mkFileName "/tmp/req_" >>> id *** (sourceHttp_ >>> evMap snd) >>> sinkFile_) -- sourceHttp_ >>> evMap snd >>> machine (putStr . decodeUtf8))
    [ arr (const loadRep)
    , openCaseLink
    -- , arr (const taskReq)
    -- , text "Nickname" "PerfTest"
    -- , arr (const loadRep)
    , arr (const logoutReq)
    ]
  -- where
  --   runIt :: MonadResource m => ProcessA (Kleisli m) (Event (ReleaseKey, Response BodyReader)) (Event FilePath, Event ByteString)
  --   runIt = proc input -> do
  --     res <- mkFileName "/tmp/req_" -< input
  --     bs <- sourceHttp_ >>> evMap snd -< _
  --     returnA -< (res, bs)

mkFileName :: ArrowApply a => FilePath -> ProcessA a (Event b) (Event FilePath, Event b)
mkFileName fpPrefix = proc input -> do
  rec
    fp <- arr mkFileName' -< (fpPrefix, n)
    n <- hold 0 >>> arr (+1) -< n <$ input
  returnA -< (fp <$ input, input)
 where
   mkFileName' (prefix, n) = prefix <> show n

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

-- Merge two events into one and fire when the right event fires
mergeEventsR :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEventsR = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  mB <- evMap Just >>> hold Nothing -< evtB
  case f mA mB of
    Nothing -> returnA -< noEvent
    Just (a, b) -> returnA -< (a, b) <$ evtB
  where
    f mA mB = (,) <$> mA <*> mB

mergeAccumL :: (ArrowApply a, Monoid c, Semigroup c) => ProcessA a (Event b, Event c) (Event (b, c))
mergeAccumL = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  acc <- evMap (<>) >>> accum mempty -< evtB
  case mA of
    Nothing -> returnA -< noEvent
    Just a -> returnA -< (a, acc) <$ evtA

mergeEvents :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEvents = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  mB <- evMap Just >>> hold Nothing -< evtB
  case f mA mB of
    Nothing -> returnA -< noEvent
    Just vals -> do
      e <- gather -< [() <$ evtA, () <$ evtB]
      returnA -< vals <$ e
  where
    f mA mB = (,) <$> mA <*> mB

mergeEvents' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEvents' = dSwitch before after
  where
    before = proc (evtA, evtB) -> do
      mA <- evMap Just >>> hold Nothing -< evtA
      mB <- evMap Just >>> hold Nothing -< evtB
      case f mA mB of
        Nothing -> returnA -< noEvent
        Just vals -> do
          e <- gather -< [() <$ evtA, () <$ evtB]
          returnA -< (vals <$ e, e)
    after _ = mergeEvents'
    f mA mB = (,) <$> mA <*> mB

(>>|) :: ArrowApply cat => ProcessA cat (Event a) (Event d) -> ProcessA cat (Event b) (Event d) -> ProcessA cat (Event (Either a b)) (Event d)
(>>|) catA catB = proc input -> do
  mV <- evMap Just >>> hold Nothing -< input
  case mV of
    Nothing -> returnA -< noEvent
    Just (Left v) -> catA >>> returnA -< v <$ input
    Just (Right v) -> catB >>> returnA -< v <$ input

infixr 5 >>|

