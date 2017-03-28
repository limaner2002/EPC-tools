{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module HTTP where

import ClassyPrelude

import MachineUtils hiding (filter, mergeEvents)
import qualified MachineUtils as MU
import qualified Control.Arrow.Machine.Misc.Discrete as D
import ParseCSV
import Data.Aeson (json, Value (..), ToJSON, FromJSON, encode, eitherDecode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Resource
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Simple (setRequestMethod, addRequestHeader, setRequestHeaders, setRequestHeader)

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
data SessionInfo = SessionInfo
  { response :: RsrcResp
  , cookies :: CookieJar
  }

mkSessionInfo :: MonadIO m => ProcessA (Kleisli m) (Event (RsrcResp, Request)) (Event SessionInfo)
mkSessionInfo = anytime mkSessionInfoK

mkSessionInfoK :: MonadIO m => Kleisli m (RsrcResp, Request) SessionInfo
mkSessionInfoK = proc (resp, req) -> do
  ct <- kleisli (const $ liftIO getCurrentTime) -< ()
  sInfo <- arr3 mkSessionInfo_ -< (ct, resp, req)
  returnA -< sInfo

mkSessionInfo_ :: UTCTime -> RsrcResp -> Request -> SessionInfo
mkSessionInfo_ ct respWithKey req = SessionInfo respWithKey' cookies
  where
    (cookies, resp') = updateCookieJar resp req ct (responseCookieJar resp)
    resp = snd respWithKey
    respWithKey' = fmap (const resp') respWithKey

updateSessionInfo :: MonadIO m => ProcessA (Kleisli m) (Event (RsrcResp, Request, SessionInfo)) (Event SessionInfo)
updateSessionInfo = anytime updateSessionInfoK

updateSessionInfoK :: MonadIO m => Kleisli m (RsrcResp, Request, SessionInfo) SessionInfo
updateSessionInfoK = proc (resp, req, sInfo) -> do
  ct <- kleisli (const $ liftIO getCurrentTime) -< ()
  sInfo' <- arr4 updateSessionInfo_ -< (ct, resp, req, sInfo)
  returnA -< sInfo'

updateSessionInfo_ :: UTCTime -> RsrcResp -> Request -> SessionInfo -> SessionInfo
updateSessionInfo_ ct respWithKey req (SessionInfo {cookies = cj}) = SessionInfo respWithKey' cookies
  where
    (cookies, resp') = updateCookieJar resp req ct (cj `mappend` responseCookieJar resp)
    resp = snd respWithKey
    respWithKey' = fmap (const resp') respWithKey

reqManager :: (Show a, FromJSON a, ToJSON a, MonadResource m) => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event (LA Value (Action a))) (Event RsrcResp)
reqManager mgr initReq loginReq = switch (login mgr initReq loginReq) (feedBack after)
  where
    after = proc input -> do
      (actEvt, sInfo) <- evMap fst &&& evMap snd -< input
      req <- evMap (id *** response) >>> updateRequest -< input -- fmap (\act -> (act, sInfo)) actEvt
      req' <- mergeEvents' >>> setReq -< (req, sInfo) -- fmap (\r -> (r, sInfo)) req
      respWithKey <- anytime (passthroughK $ print . fst) >>> makeRequest -< fmap (\r -> (r, mgr)) req'

      x <- mergeEvents' -< (respWithKey, req')
      sInfo' <- mergeEvents' >>> evMap (\((x,y), z) -> (x, y, z)) >>> updateSessionInfo -< (x, sInfo) -- fmap (\(x, y) -> (x, y, sInfo)) x
      res <- mergeEvents' -< (respWithKey, sInfo')
      returnA -< res
  -- where
  --   after sInit = proc actEvt -> do
  --     rec
  --       req <- evMap (id *** response) >>> updateRequest -< fmap (\act -> (act, sInfo)) actEvt
  --       req' <- setReq -< fmap (\r -> (r, sInfo)) req
  --       respWithKey <- anytime (passthroughK $ print . fst) >>> makeRequest -< fmap (\r -> (r, mgr)) req'

  --       x <- mergeEvents' -< (respWithKey, req')
  --       sInfo' <- updateSessionInfo -< fmap (\(x, y) -> (x, y, sInfo)) x
  --       sInfo <- hold sInit -< sInfo'
  --     returnA -< respWithKey

    -- This is causing the problem as well as updateRequest
setReq :: MonadResource m => ProcessA (Kleisli m) (Event (Request, SessionInfo)) (Event Request)
setReq = proc input -> do
  csrfToken <- evMap (snd . response . snd) >>> csrfTokenHeader -< input
  req' <- evMap (id *** arr (cookies)) >>> machine (uncurry nowInsert) >>> evMap fst -< input
  req'' <- mergeEvents' >>> anytime insertHeader -< (csrfToken, req')
  returnA -< req''

login :: MonadResource m => Manager -> Request -> Request -> ProcessA (Kleisli m) (Event a) (Event RsrcResp, Event SessionInfo)
login mgr initReq loginReq = proc a -> do
  sInfo <- makeRequest &&& evMap fst >>> mergeEvents' >>> mkSessionInfo -< (initReq, mgr) <$ a
  csrfToken <- evMap (snd . response) >>> csrfTokenHeader -< sInfo

  req' <- id *** evMap cookies >>> mergeEvents' >>> machine (uncurry nowInsert) >>> evMap fst -< (loginReq <$ sInfo, sInfo)
  req'' <- mergeEvents' >>> anytime insertHeader -< (csrfToken, req')
  req''' <- mergeEvents' >>> evMap (uncurry appendQueryString) -< (csrfToken, req'')

  respWithKey <- makeRequest -< fmap (\req -> (req, mgr)) req'''

  x <- mergeEvents' -< (respWithKey, req''')
  sInfo' <- mergeEvents' >>> evMap (\((x, y), z) -> (x, y, z)) >>> updateSessionInfo -< (x, sInfo)

  returnA -< (respWithKey, sInfo') -- (respWithKey, respWithKey)

    -- This is causing the problem as well as setReq
updateRequest :: (Show a, ToJSON a, FromJSON a, MonadResource m) => ProcessA (Kleisli m) (Event (LA Value (Action a), RsrcResp)) (Event Request)
updateRequest =
  evMap fst &&& (evMap snd >>> sourceHttp_ >>> evMap snd >>> machineParser appianResponseParser)
  >>> mergeEvents'
  >>> evMap parseResp
  -- >>> evMap (\(act, resp) -> runLA act resp)
  -- evMap (\(act, _) -> runLA act (Object mempty))
  >>> MU.fork
  >>> machine mkUpdateRequest
  where
    parseResp (act, Left exc) = runLA act (Object mempty)
    parseResp (act, Right resp) = runLA act resp

testIt :: IO ()
testIt = do
  mgr <- newManager tlsManagerSettings
  siteReq <- parseRequest "https://portal-preprod.usac.org"
  loginReq <- authReq baseUrl "marjorie.allen@sl.universalservice.org" "USACpreprod1!" mempty

  let loadRep = mkHRef "/suite/rest/a/uicontainer/latest/YB7oUg/view" "GET" mempty Nothing :: Action Text
      logoutReq = mkHRef "/suite/logout" "GET" mempty Nothing :: Action Text
      caseReq = mkHRef "https://portal-preprod.usac.org/suite/api/tempo/open-a-case/action/ksBnWUR1XjkLMmEri-oDn0gL2Fmrr_v3Fcs3gqyQpKrFxfzkjtesbm7cYTL9ZE8wYQ1dhVXb6DJV6r4NQGEX3geSw_O-io80YMwqMk" "GET" mempty Nothing :: Action Text
      taskReq = mkHRef "https://portal-preprod.usac.org/suite/rest/a/uicontainer/latest/report/YB7oUg" "GET" mempty (Just "application/atom+json,application/json")
      taskReq' = mkHRef "https://portal-preprod.usac.org/suite/rest/a/model/latest/4064" "POST" "{}" (Just "application/vnd.appian.tv.ui+json")
      tasksReq = mkHRef "/suite/api/feed/tempo?m=menu-tasks&t=t&s=pt&defaultFacets=%5Dstatus-open%5D" "GET" mempty (Just "application/json")
      broadReq = mkHRef "/suite/api/groups/broadcast-targets" "GET" mempty (Just "application/json")

  runRMachine_ (reqManager mgr siteReq loginReq >>> evMap (responseStatus . snd) >>> machine print) -- mkFileName "/tmp/req_" >>> id *** (sourceHttp_ >>> evMap snd) >>> sinkFile_) -- sourceHttp_ >>> evMap snd >>> machine (putStr . decodeUtf8))
    [ arr (const broadReq)
    , arr (const tasksReq)
    , getAttrLinks
    -- , arr (const taskReq)
    -- , openCaseLink
    -- , arr (const taskReq')
    -- , text "Nickname" "PerfTest"
    -- , arr (const loadRep)
    , arr (const logoutReq)
    ]

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
  <$> addRequestHeaders baseHeaders
  <$> setQueryString (params <> authParams (encodeUtf8 un) (encodeUtf8 pw))
  <$> parseUrlThrow (baseUrl <> "/suite/auth?appian_environment=tempo")

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
mkUpdateRequest (HRef url method body mAccept)
  | isPrefixOf baseUrl url == False = do
      req <- parseRequest . unpack $ baseUrl <> url
      return $ setAccept $ req { method = encodeUtf8 method
                   , requestBody = RequestBodyBS body
                   , requestHeaders = baseHeaders
                   }
  | otherwise = do
      req <- parseRequest $ unpack url
      return $ setAccept $ req { method = encodeUtf8 method
                   , requestBody = RequestBodyBS body
                   , requestHeaders = baseHeaders
                   }
 where
   setAccept = case mAccept of
     Nothing -> id
     Just accept -> setRequestHeader (CI.mk "Accept") [accept]
mkUpdateRequest upd@(Update {taskId = taskId}) = do
  req <- parseRequest . unpack $ "/suite/rest/a/task/latest/" <> show taskId <> "/form"
  return $ req { requestBody = RequestBodyLBS (encode upd)
               , method = "POST"
               , requestHeaders = baseHeaders
               }

-- -- Merge two events into one and fire when the left event fires
-- mergeEvents' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
-- mergeEvents' = proc (evtA, evtB) -> do
--   mA <- evMap Just >>> hold Nothing -< evtA
--   mB <- evMap Just >>> hold Nothing -< evtB
--   case f mA mB of
--     Nothing -> returnA -< noEvent
--     Just (a, b) -> returnA -< (a, b) <$ evtA
--   where
--     f mA mB = (,) <$> mA <*> mB

-- -- Merge two events into one and fire when the right event fires
-- mergeEvents' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
-- mergeEvents' = proc (evtA, evtB) -> do
--   mA <- evMap Just >>> hold Nothing -< evtA
--   mB <- evMap Just >>> hold Nothing -< evtB
--   case f mA mB of
--     Nothing -> returnA -< noEvent
--     Just (a, b) -> returnA -< (a, b) <$ evtB
--   where
--     f mA mB = (,) <$> mA <*> mB

-- mergeAccumL :: (ArrowApply a, Monoid c, Semigroup c) => ProcessA a (Event b, Event c) (Event (b, c))
-- mergeAccumL = proc (evtA, evtB) -> do
--   mA <- evMap Just >>> hold Nothing -< evtA
--   acc <- evMap (<>) >>> accum mempty -< evtB
--   case mA of
--     Nothing -> returnA -< noEvent
--     Just a -> returnA -< (a, acc) <$ evtA

-- mergeEvents :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
-- mergeEvents = proc (evtA, evtB) -> do
--   mA <- evMap Just >>> hold Nothing -< evtA
--   mB <- evMap Just >>> hold Nothing -< evtB
--   case f mA mB of
--     Nothing -> returnA -< noEvent
--     Just vals -> do
--       e <- gather -< [() <$ evtA, () <$ evtB]
--       returnA -< vals <$ e
--   where
--     f mA mB = (,) <$> mA <*> mB

-- mergeEvents' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
-- mergeEvents' = dSwitch before after
--   where
--     before = proc (evtA, evtB) -> do
--       mA <- evMap Just >>> hold Nothing -< evtA
--       mB <- evMap Just >>> hold Nothing -< evtB
--       case f mA mB of
--         Nothing -> returnA -< noEvent
--         Just vals -> do
--           e <- gather -< [() <$ evtA, () <$ evtB]
--           returnA -< (vals <$ e, e)
--     after _ = mergeEvents'
--     f mA mB = (,) <$> mA <*> mB

(>>|) :: ArrowApply cat => ProcessA cat (Event a) (Event d) -> ProcessA cat (Event b) (Event d) -> ProcessA cat (Event (Either a b)) (Event d)
(>>|) catA catB = proc input -> do
  mV <- evMap Just >>> hold Nothing -< input
  case mV of
    Nothing -> returnA -< noEvent
    Just (Left v) -> catA >>> returnA -< v <$ input
    Just (Right v) -> catB >>> returnA -< v <$ input

infixr 5 >>|

arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry

arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 f = arr (\(x, y, z) -> f x y z)

arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 f = arr (\(b, c, d, e) -> f b c d e)

(<<*>>) :: ArrowApply a => ProcessA a (Event b) (Event (c -> d)) -> ProcessA a (Event b) (Event c) -> ProcessA a (Event b) (Event d)
(<<*>>) fA fB = proc input -> do
  f <- fA -< input
  x <- fB -< input
  h <- mergeEvents' >>> evMap (\(f', x') -> f' x') -< (f, x)
  returnA -< h

infixr 5 <<*>>

feedBack :: Monad m => ProcessA (Kleisli m) (Event (b, c)) (Event (d, c)) -> c -> ProcessA (Kleisli m) (Event b) (Event d)
feedBack f init = constructT kleisli0 go
  where
    go = do
      b <- await
      c <- lift $ runKleisli (run f) [(b, init)]
      loop c
    loop c = do
      mapM_ (yield . trace "yielding" . fst) c
      b <- await
      c' <- lift $ runKleisli (run f) $ zip (repeat b) (fmap snd c)
      loop c'

testFB :: Monad m => ProcessA (Kleisli m) (Event Int) (Event Int)
testFB = feedBack addThem 0
  where
    addThem = proc input -> do
      x <- evMap (uncurry (+)) >>> evMap (\x -> (x, x)) -< input
      returnA -< x

-- -- Merge two events into one and fire when the right event fires
-- mergeEvents'' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
-- mergeEvents'' = proc (evtA, evtB) -> do
--   mA <- evMap Just >>> hold Nothing -< evtA
--   mB <- evMap Just >>> hold Nothing -< evtB
--   case f mA mB of
--     Nothing -> returnA -< noEvent
--     Just (a, b) -> returnA -< (a, b) <$ evtB
--   where
--     f mA mB = (,) <$> mA <*> mB

mergeEvents' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEvents' = proc (evtA, evtB) -> do
  mA <- evMap Just >>> D.hold Nothing -< evtA
  mB <- evMap Just >>> D.hold Nothing -< evtB
  res <- D.arr2 f >>> D.edge >>> filterJust -< (mA, mB)
  returnA -< res
  where
    f mA mB = (,) <$> mA <*> mB
