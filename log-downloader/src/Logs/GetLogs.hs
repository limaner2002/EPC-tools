{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}

module Logs.GetLogs where

import ClassyPrelude
import Data.ByteString.Streaming.HTTP
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Data.CaseInsensitive
import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as S
import Control.Arrow
import Logs.Core
import System.FilePath
import Control.Monad.Logger

data LogSettings = LogSettings
  { username :: Text
  , password :: Text
  , nodeNames :: [Text]
  , logName :: FilePath
  , logDestination :: FilePath
  , logUrl :: Text
  } deriving Show

    -- Everything below should be in a separate module

getResp :: MonadResource m => Manager -> String -> m (Response (BSS.ByteString m ()))
getResp mgr baseUrl = do
  req <- addRequestHeaders baseHeaders <$> parseRequest baseUrl
  http req mgr

getCSRFToken :: MonadThrow m => Response body -> m (ByteString, ByteString)
getCSRFToken = getResponseCookie "__appianCsrfToken" >>> fmap cookie_value >>> mkHeader
  where
    mkHeader Nothing = throwM $ NoTokenException "There is no CSRF token present!"
    mkHeader (Just v) = return ("X-APPIAN-CSRF-TOKEN", v)

data NoTokenException = NoTokenException Text

instance Show NoTokenException where
  show (NoTokenException msg) = "NoTokenException: " <> show msg

instance Exception NoTokenException

baseHeaders :: RequestHeaders
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

login :: (MonadResource m, MonadLogger m) => Manager -> Maybe ByteString -> ByteString -> ByteString -> String -> m (Response (BSS.ByteString m ()))
login mgr nodeName un pw baseUrl = do
  resp <- getResp mgr baseUrl
  req <- setRequestMethod POST <$> addRequestHeader "Content-Type" "application/x-www-form-urlencoded" <$> parseRequest (baseUrl </> "suite/auth?appian_environment=tempo")

  let mCJ = join $ fmap (\nn -> setNode nn resp) nodeName

  (req', _) <- case mCJ of
    Nothing -> do
      logErrorN "Either this is not a multi-node environment or the cookie format has changed."
      insertCookiesIntoRequest req (responseCookieJar resp) <$> liftBase getCurrentTime
    Just cj -> insertCookiesIntoRequest req cj <$> liftBase getCurrentTime

  (n, v) <- getCSRFToken resp

  let mkReq = addRequestHeaders baseHeaders >>> setQueryString ((n, Just v) : authParams un pw) >>> uncurry addRequestHeader csrfHeader
      req'' = mkReq req'
      csrfHeader = (mk n, v)

  http req'' mgr

addRequestHeaders headers req = req { requestHeaders = headers <> requestHeaders req }

addRequestHeader headerName val req = req { requestHeaders = (headerName, val) : requestHeaders req }

setRequestMethod :: StdMethod -> Request -> Request
setRequestMethod meth req = req { method = encodeUtf8 (tshow meth) }

printResponse :: MonadIO m => S.Stream (S.Of (Response (BSS.ByteString m b))) m r -> m r
printResponse = S.map responseBody >>> S.mapM_ BSS.stdout

loginReq :: MonadThrow m => ByteString -> ByteString -> String -> m Request
loginReq un pw url = setQueryString (authParams un pw) <$> addRequestHeaders baseHeaders <$> parseRequest url

downloadLogs :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadThrow m) => Text -> Text -> [Text] -> [FilePath] -> FilePath -> String -> m ()
downloadLogs un pw nodes logs dir baseUrl = do
  mgr <- liftIO $ newManager tlsManagerSettings
  runResourceT $ traverse_
    (\(nodeName, logNames) -> do
        loginResp <- login mgr (Just $ encodeUtf8 nodeName) (encodeUtf8 un) (encodeUtf8 pw) baseUrl
        let f = S.each >>> mkManager >>> S.map flatten3 >>> reqMgr (loginResp, responseCookieJar loginResp, printNode)
            downloadUrls = fmap ((baseUrl </> "suite/logs") </>) logNames
            fileSink logName = S.map responseBody >>> S.mapM_ (BSS.writeFile (dir </> takeFileName logName <> "." <> unpack nodeName))
        reqs <- mapM parseRequest downloadUrls
        logoutReq <- parseRequest $ baseUrl </> "suite/logout"
        f $ zip reqs (fmap fileSink logNames) `snoc` (logoutReq, printStatus)
    ) $ zip nodes (repeat logs)
    where
      printStatus = S.map responseStatus >>> S.mapM_ (logInfoN . tshow)
      printNode = S.map getNode >>> S.concat >>> S.map (\node -> "Logged into " <> decodeUtf8 node) >>> S.mapM_ logInfoN

flatten3 :: (a, (b, c)) -> (a, b, c)
flatten3 (a, (b, c)) = (a, b, c)
