{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Execute
  ( schedule
  , doIfDirIsEmpty
  , runJMeter
  ) where

import ClassyPrelude
import System.Directory
import System.FilePath ((</>))
import qualified System.IO as SIO
import GHC.IO.Exception
import Data.Time
import Types

       -- New process imports
import Data.Streaming.Process hiding (runCommand)
import Streaming hiding ((<>))
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as BSS
import qualified Data.ByteString.Streaming.Char8 as SC8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Arrow

createCommand :: JMeterOpts -> NUsers -> CmdSpec
createCommand (JMeterOpts _ _ jmxPath jmeterPath _ otherOpts _) (NUsers n) =
--   RawCommand jmeterPath []
  RawCommand jmeterPath $
  [ "-n"
  , "-t"
  , jmxPath
  , "-Jusers=" <> show n
  , "-JoutputFile=aggregate_" <> show n <> "_" <> show n <> ".csv"
  , "-JsessionPrefix=session_"
  , "-JloopCount=1"
  ] <> fmap unpack otherOpts

readRun :: Text -> Maybe Run
readRun = fmap Run . readMay

readNUsers :: Text -> Maybe NUsers
readNUsers = fmap NUsers . readMay

newCP :: CmdSpec -> CreateProcess
newCP cs = (shell mempty) {cmdspec = cs}

streamConsumer :: (MonadIO m, MonadBase IO m) => TimeZone -> TVar UTCTime -> Handle -> m ()
streamConsumer tz var =
      SC8.fromHandle
  >>> SC8.lines
  >>> mapped SC8.toStrict
  >>> S.mapM (addTime tz)
  >>> S.mapM (updateLastSeen var)
  >>> S.mapM_ (liftBase . C8.putStrLn)

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
        True -> liftBase $ interruptProcessGroupOf $ streamingProcessHandleRaw sHandle
        False -> loop

handleKill :: MonadBase IO m => StreamingProcessHandle -> TMVar Int -> TVar Action -> m ()
handleKill sHandle tmv actionT = liftBase $ loop
  where
    loop = do
      res <- atomically $ do
        takeTMVar tmv
        writeTVar actionT Stop
        return "Wrote stop to TVar"
      print res
      terminateProcess $ streamingProcessHandleRaw sHandle
      loop

runCommand
  :: (Forall (Pure m),
      MonadIO m, MonadBaseControl IO m) =>
     TMVar Int -> TVar Action -> CreateProcess -> m ()
runCommand tmVar actionT cp = do
  (ClosedStream, out, err, cph) <- streamingProcess cp
  tz <- liftIO getCurrentTimeZone
  var <- liftIO $ newTVarIO =<< getCurrentTime

  _ <- runConcurrently
     $ Concurrently (streamConsumer tz var out)
    *> Concurrently (streamConsumer tz var err)
--     *> Concurrently (checkLastSeen input cph var 300)
    *> Concurrently (race (handleKill cph tmVar actionT)
                          (do
                              res <- waitForStreamingProcess cph
                              print res
                          )
                    )
  atomically $ do
    empty <- isEmptyTMVar tmVar
    case empty of
      True -> return ()
      False -> do
        _ <- takeTMVar tmVar
        return ()

    -- EPC-tools specific stuff
isEmptyDirectory [] = True
isEmptyDirectory _ = False

runJMeter_ :: TMVar Int -> TVar Action -> JMeterOpts -> IO ()
runJMeter_ tmVar actionT opts = mapM_ runForNUsers (opts ^. nUsers)
  where
    createNUsersDir (NUsers u) = do
      exists <- doesDirectoryExist $ show u
      case exists of
        True -> putStrLn $ "Directory: " <> tshow u <> " already exists!"
        False -> createDirectory $ show u
    runForNUsers user@(NUsers u) = do
      action <- atomically $ readTVar actionT
      case action of
        Stop -> putStrLn "This job has been cancelled. Not continuing."
        Continue -> do
          createNUsersDir user
          setCurrentDirectory $ show u
          mapM_ (run user) [1..(extractRun $ opts ^. nRuns)]
          putStrLn "runForNUsers: Moving to parent"
          setCurrentDirectory "../"
    run u r = do
      action <- atomically $ readTVar actionT
      case action of
        Stop -> putStrLn "This job has been cancelled. Not continuing."
        Continue -> do
          let newDir = "Run " <> show r
              cmd = createCommand opts u
          createDirectory newDir
          setCurrentDirectory newDir
          delayJob $ opts ^. sleepTime
          putStrLn $ "Running command: " <> (pack $ showCmdSpec cmd)
          res <- tryAny $ runCommand tmVar actionT (newCP cmd)
          case res of
            Left exc -> print exc
            Right x -> return ()
          putStrLn "run: Moving to parent"
          setCurrentDirectory "../"
    extractRun (Run n) = n
    extractUser (NUsers u) = u

runJMeter :: TMVar Int -> JMeterOpts -> IO ()
runJMeter tmVar run = do
  createDirectoryIfMissing False $ unpack . fromRunName $ run ^. runName
  setCurrentDirectory $ unpack . fromRunName $ run ^. runName
  actionT <- newTVarIO Continue
  res <- tryAny $ runJMeter_ tmVar actionT run
  case res of
    Left exc -> print exc
    Right x -> return x
  putStrLn "runJMeter: Moving to parent"
  setCurrentDirectory "../"
  delayJob $ run ^. sleepTime

runningMessage :: JMeterOpts -> Text
runningMessage (JMeterOpts n users jmxPath jmeterPath runName otherOpts sleepTime) = do
  "Running " <> tshow jmeterPath <> " using " <> tshow jmxPath <> " with " <> tshow n <> " runs with " <> tshow users <> " users."
    <> "\notherOpts: " <> intercalate " " otherOpts
    <> "\n" <> tshow runName
    <> "\nwith " <> tshow sleepTime <> " seconds before the next run."

showCmdSpec (RawCommand bin opts) = bin <> " " <> intercalate " " opts
showCmdSpec (ShellCommand cmd) = cmd

schedule :: ScheduledTime -> IO () -> IO ()
schedule scheduledTime action = loop
  where
    loop = do
      ct <- getCurrentTime
      case ct >= time of
        True -> do
          putStrLn "Running job now"
          action
        False -> do
          threadDelay 1000000
          loop
    time = fromScheduledTime scheduledTime

doIfDirIsEmpty :: IO () -> IO ()
doIfDirIsEmpty act = do
  l <- listDirectory =<< getCurrentDirectory
  case isEmptyDirectory l of
    False -> fail $ "The current directory is not empty. Either change to an empty directory or remove everything from the current one."
    True -> act

delayJob :: JobDelay -> IO ()
delayJob (Delay n) = do
  putStrLn $ "Waiting for " <> tshow n <> " seconds"
  threadDelay (n * 1000000)
  putStrLn "Starting the next job."
delayJob (AtTime t) = do
  putStrLn $ "Sleeping until " <> tshow t <> "."
  tz <- getCurrentTimeZone
  ct <- getCurrentTime
  schedule (mkScheduledTime tz t ct) $ putStrLn "Starting the next job."
