{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Logs.Core where

import ClassyPrelude
import Data.ByteString.Streaming.HTTP
import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as S
import Control.Arrow

updateCookies :: MonadIO m => Request -> Maybe (CookieJar) -> m Request
updateCookies req Nothing = pure req
updateCookies req (Just cj) = do
  ct <- liftIO getCurrentTime
  return $ fst $ insertCookiesIntoRequest req cj ct

    -- Takes the response and the request that created the response and returns a cookie jar
cookieManager :: MonadIO m => S.Stream (S.Of (Request, Response body)) m r -> S.Stream (S.Of CookieJar) m r
cookieManager = S.scanM f (pure mempty) pure

f :: MonadIO m => CookieJar -> (Request, Response body) -> m CookieJar
f cj (req, resp) = do
  ct <- liftIO getCurrentTime
  let cj' = responseCookieJar resp
  return $ mappend cj $ fst $ updateCookieJar resp req ct cj'

type Resp m = Response (BSS.ByteString m ())
type RespConsumer m = S.Stream (S.Of (Resp m)) m () -> m ()

-- Does not appear to work correctly.
reqMgr :: (MonadIO m, MonadResource m) => (Resp m, CookieJar, RespConsumer m) -> S.Stream (S.Of (Manager, Request, RespConsumer m)) m () -> m ()
reqMgr init = S.scanM updateCookies' (pure init) pure >>> S.mapM_ consumeIt
  where
    updateCookies' (_, cj, _) (mgr, req, consumer) = do
      req' <- updateCookies req (Just cj)
      resp <- http req' mgr
      cj' <- f cj (req, resp)
      return (resp, cj', consumer)
    consumeIt (resp, _, consumer) = S.yield >>> consumer $ resp

chanIn :: MonadIO m => TChan (Maybe b) -> S.Stream (S.Of b) m ()
chanIn chan = go
  where
    go = do
      mVal <- atomically $ readTChan chan
      case mVal of
        Nothing -> return ()
        Just v -> do
          S.yield v
          go

chanOut :: MonadIO m => TChan (Maybe b) -> S.Stream (S.Of b) m r -> m r
chanOut chan = S.mapM_ (atomically . writeTChan chan . Just)

concurrentMerge :: (MonadIO m, MonadBaseControl IO m, Forall (Pure m)) => [S.Stream (S.Of a) m r] -> S.Stream (S.Of a) m ()
concurrentMerge streams = do
  chan <- lift . liftBase $ newTChanIO
  lift . fork . void $ mapConcurrently (chanOut chan) streams
  chanIn chan

keepManager :: (Manager, Maybe a) -> a -> (Manager, Maybe a)
keepManager (mgr, _) a = (mgr, Just a)

mkManager :: MonadIO m => S.Stream (S.Of a) m r -> S.Stream (S.Of (Manager, a)) m r
mkManager stream = do
  mgr <- lift . liftIO $ newManager tlsManagerSettings
  S.scan keepManager (mgr, Nothing) sequence >>> S.mapMaybe id $ stream
  
getNode :: Response body -> Maybe ByteString
getNode = getResponseCookie "JSESSIONID"
  >>> fmap cookie_value
  >>> fmap (splitSeq ".")
  >>> mapM toMaybe >>> join
   where
     toMaybe [_, node] = Just node
     toMaybe _ = Nothing

getResponseCookie :: ByteString -> Response body -> Maybe Cookie
getResponseCookie name = responseCookieJar
  >>> destroyCookieJar
  >>> find (\c -> cookie_name c == name)

setNode :: ByteString -> Response body -> Maybe CookieJar
setNode nodeName resp = do
  cookie <- getResponseCookie "JSESSIONID" resp
  cookieVal <- cookie_value >>> splitSeq "." >>> setNode' $ cookie

  let cookie' = setCookieValue cookie cookieVal
      (_, cj') = removeExistingCookieFromCookieJar cookie cj
      cj'' = insertCheckedCookie cookie' cj' False
  return cj''
    where
      setNode' [hash, _] = Just $ hash <> "." <> nodeName
      setNode' _ = Nothing
      cj = responseCookieJar resp
      setCookieValue c v = c {cookie_value = v}

