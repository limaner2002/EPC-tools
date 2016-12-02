{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Execute where

import ClassyPrelude
import System.Directory
import System.FilePath ((</>))
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Process hiding (runCommand)
import System.Process hiding (runCommand)
import qualified System.IO as SIO
import GHC.IO.Exception
import Data.Time
import Control.Concurrent.Async.Lifted
import Types

createCommand :: JMeterOpts -> NUsers -> CmdSpec
createCommand (JMeterOpts _ _ jmxPath jmeterPath _ otherOpts) (NUsers n) =
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

streamConsumer = CC.mapM_ (\bs -> putStr (decodeUtf8 bs) >> SIO.hFlush (stdout))

-- runCommand :: CreateProcess -> IO ()
runCommand :: CreateProcess -> IO (ExitCode, (), ())
-- runCommand cp = sourceProcessWithStreams cp CC.sinkNull streamConsumer streamConsumer
runCommand _ = do
  putStrLn "Executing process"
  return (ExitSuccess, (), ())

isEmptyDirectory [] = True
isEmptyDirectory _ = False

runJMeter :: JMeterOpts -> IO ()
runJMeter opts@(JMeterOpts n users _ jmeterPath _ _) = mapM_ runForNUsers users
  where
    createNUsersDir (NUsers u) = do
      exists <- doesDirectoryExist $ show u
      case exists of
        True -> putStrLn $ "Directory: " <> tshow u <> " already exists!"
        False -> createDirectory $ show u
    runForNUsers user@(NUsers u) = do
      createNUsersDir user
      setCurrentDirectory $ show u
      mapM_ (run user) [1..(extractRun n)]
      setCurrentDirectory "../"
    run u r = do
      let newDir = "Run " <> show r
          cmd = createCommand opts u
      createDirectory newDir
      setCurrentDirectory newDir
      putStrLn $ "Running command: " <> (pack $ showCmdSpec cmd)
      runCommand (newCP cmd)
      putStrLn "Waiting for 1 minute"
      threadDelay 60000000
      setCurrentDirectory "../"
    extractRun (Run n) = n
    extractUser (NUsers u) = u

batchJMeterScripts :: [JMeterOpts] -> IO ()
batchJMeterScripts runs = doIfDirIsEmpty $ mapM_ go runs
  where
    go run = do
      createDirectoryIfMissing False $ unpack . fromRunName $ runName run
      setCurrentDirectory $ unpack . fromRunName $ runName run
      runJMeter run
      setCurrentDirectory "../"

runningMessage :: JMeterOpts -> Text
runningMessage (JMeterOpts n users jmxPath jmeterPath runName otherOpts) = do
  "Running " <> tshow jmeterPath <> " using " <> tshow jmxPath <> " with " <> tshow n <> " runs with " <> tshow users <> " users."
    <> "\notherOpts: " <> intercalate " " otherOpts
    <> "\n" <> tshow runName

showCmdSpec (RawCommand bin opts) = bin <> " " <> intercalate " " opts
showCmdSpec (ShellCommand cmd) = cmd

schedule :: UTCTime -> IO () -> IO ()
schedule time action = do
  ct <- getCurrentTime
  case ct >= time of
    True -> do
      putStrLn "Running job now"
      action
    False -> do
      threadDelay 1000000
      schedule time action

doIfDirIsEmpty :: IO () -> IO ()
doIfDirIsEmpty act = do
  l <- listDirectory =<< getCurrentDirectory
  case isEmptyDirectory l of
    False -> fail $ "The current directory is not empty. Either change to an empty directory or remove everything from the current one."
    True -> act
