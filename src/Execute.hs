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

data ExeOpts = ExeOpts
  { nRuns :: Run
  , nUsers :: [NUsers]
  , jmxPath :: FilePath
  , jmeterPath :: FilePath
  } deriving Show

createCommand :: ExeOpts -> NUsers -> CmdSpec
createCommand (ExeOpts _ _ jmxPath jmeterPath) (NUsers n) =
  RawCommand jmeterPath
  [ "-n"
  , "-t"
  , jmxPath
  , "-Jusers=" <> show n
  , "-JoutputFile=aggregate_" <> show n <> "_" <> show n <> ".csv"
  , "-JsessionPrefix=session_"
  , "-JloopCount=1"
  ]

newtype NUsers = NUsers Int
  deriving (Show, Eq)

newtype Run = Run Int
  deriving (Show, Eq)

readRun :: Text -> Maybe Run
readRun = fmap Run . readMay

readNUsers :: Text -> Maybe NUsers
readNUsers = fmap NUsers . readMay

newCP :: CmdSpec -> CreateProcess
newCP cs = (shell mempty) {cmdspec = cs}

streamConsumer = CC.mapM_ (\bs -> putStr (decodeUtf8 bs) >> SIO.hFlush (stdout))

-- runCommand :: CreateProcess -> IO ()
runCommand :: CreateProcess -> IO (ExitCode, (), ())
runCommand cp = sourceProcessWithStreams cp CC.sinkNull streamConsumer streamConsumer

isEmptyDirectory [] = True
isEmptyDirectory _ = False

runJMeter :: ExeOpts -> IO ()
runJMeter opts@(ExeOpts n users _ jmeterPath) = go
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
    go = do
      l <- listDirectory =<< getCurrentDirectory
      case isEmptyDirectory l of
        False -> fail $ "The current directory is not empty. Either change to an empty directory or remove everything from the current one."
        True -> mapM_ runForNUsers users
    extractRun (Run n) = n
    extractUser (NUsers u) = u

runningMessage :: ExeOpts -> Text
runningMessage (ExeOpts n users jmxPath jmeterPath) = do
  "Running " <> tshow jmeterPath <> " using " <> tshow jmxPath <> " with " <> tshow n <> " runs with " <> tshow users <> " users."

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

