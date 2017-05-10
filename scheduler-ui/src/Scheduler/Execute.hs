{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Scheduler.Execute where

import ClassyPrelude
import GHC.IO.Exception
import Data.Streaming.Process hiding (runCommand)
import Streaming hiding ((<>))
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as BSS
import qualified Data.ByteString.Streaming.Char8 as SC8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Arrow
import Data.Constraint hiding ((&&&))
import Data.Time
import qualified System.IO as SIO

createCommand :: FilePath -> FilePath -> CmdSpec
createCommand jmeterPath jmxPath =
  RawCommand jmeterPath
  [ "-n"
  , "-t"
  , jmxPath
  ]

newCP :: CmdSpec -> CreateProcess
newCP cs = (shell mempty) {cmdspec = cs}

streamConsumer :: (MonadIO m, MonadBase IO m) => TimeZone -> TVar (UTCTime) -> Handle -> m ()
streamConsumer tz var = SC8.fromHandle >>> SC8.lines >>> mapped SC8.toStrict >>> S.mapM (addTime tz) >>> S.mapM (updateLastSeen var) >>> S.mapM_ (liftBase . C8.putStrLn)

addTime :: MonadBase IO m => TimeZone -> ByteString -> m ByteString
addTime tz bs = do
  ct <- liftBase $ utcToZonedTime tz <$> getCurrentTime
  return $ encodeUtf8 (pack $ formatTime defaultTimeLocale "%F %T %Z" ct) <> ":\t" <> bs

updateLastSeen :: MonadBase IO m => TVar UTCTime -> ByteString -> m ByteString
updateLastSeen var str = do
  ct <- liftBase getCurrentTime
  liftBase . atomically $ writeTVar var ct
  return str

type Secs = Int

checkLastSeen :: MonadBase IO m => Handle -> StreamingProcessHandle -> TVar UTCTime -> Secs -> m ()
checkLastSeen input sHandle var delay = loop
  where
    loop = do
      liftBase $ threadDelay (delay * 1000000)
      t <- liftBase $ atomically $ readTVar var
      ct <- liftBase getCurrentTime

      let tDelta = diffUTCTime ct t

      case tDelta > fromIntegral delay of
        True -> do
          trace "Terminating process and exiting" $ liftBase $ interruptProcessGroupOf $ streamingProcessHandleRaw sHandle
        False -> loop

runCommand
  :: (Forall (Pure m),
      MonadIO m, MonadBaseControl IO m) =>
     CreateProcess -> m ()
runCommand cp = do
  (input, out, err, cph) <- streamingProcess cp
  tz <- liftIO getCurrentTimeZone
  var <- liftIO $ newTVarIO =<< getCurrentTime

  runConcurrently
     $ Concurrently (streamConsumer tz var out)
    *> Concurrently (streamConsumer tz var err)
    *> Concurrently (checkLastSeen input cph var 300)
    *> Concurrently ( do
                        res <- waitForStreamingProcess cph
                        print res
                    )
