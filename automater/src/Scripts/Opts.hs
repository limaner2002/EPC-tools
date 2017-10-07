{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.Opts -- where
  ( commandsInfo
  , runScript
  , Script (..)
  , loggingFunc
  ) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types

import Scripts.FCCForm471
import Scripts.CreateCSCase
import Scripts.FCCForm486
import Scripts.SPINChangeIntake
import Scripts.InitialReview
import Scripts.ReviewCommon
import Scripts.AdminReview
import Scripts.AdminIntake
import Appian.Client (runAppian, LogMessage(..), logChan)
import Appian.Instances
import Appian.Types (AppianUsername (..))
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Arrow
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource hiding (throwM)
import Data.Aeson (Value)
import Control.Lens
import Network.HTTP.Client
import Appian
import Stats.CsvStream
import Control.Monad.Logger
import Scripts.Common

getPassword :: IO String
getPassword = pure "EPCPassword123!"

runScript :: Script -> BaseUrl -> String -> FilePath -> Int -> IO ()
runScript (CSScript script) baseUrl username fp nThreads = do
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  password <- getPassword
  let login = Login (pack username) (pack password)
  (_, res) <- concurrently (loggingFunc fp)
              ( do
                  atomically $ writeTChan logChan $ Msg $ "timeStamp,elapsed,label,responseCode"
                  results <- mapConcurrently (const $ tryAny $ runAppian script (ClientEnv mgr baseUrl) login) $ [1..nThreads]
                  atomically $ writeTChan logChan Done
                  return results
              )
  dispResults res
runScript (Form471 script) baseUrl username fp nThreads = do
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  password <- getPassword
  logins <- S.fst' <$> (csvStreamByName >>> S.drop 10 >>> S.toList >>> runResourceT >>> runNoLoggingT $ "applicantConsortiums.csv")
  (_, res) <- concurrently (loggingFunc fp)
              ( do
                  atomically $ writeTChan logChan $ Msg $ "timeStamp,elapsed,label,responseCode"
                  results <- mapConcurrently (\login -> tryAny $ runAppian script (ClientEnv mgr baseUrl) login) $ take nThreads logins
                  atomically $ writeTChan logChan Done
                  return results
              )
  dispResults res
runScript (Form486 script userFile) baseUrl _ fp nThreads = do
  stderrLn $ intercalate " " [pack userFile, tshow baseUrl, pack fp, tshow nThreads]
  stderrLn "\n********************************************************************************\n"
  stderrLn "Waiting for 2 mins. Does the above look correct?"
  threadDelay (120000000)
  stderrLn "Starting 486 intake now!"
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  logins <- S.fst' <$> (csvStreamByName >>> S.toList >>> runResourceT >>> runNoLoggingT $ userFile)
  (_, res) <- concurrently (loggingFunc fp)
              (do
                  atomically $ writeTChan logChan $ Msg "timeStamp,elapsed,label,responseCode"
                  results <- mapConcurrently (\login -> tryAny $ runAppian (script $ login ^. username . to AppianUsername) (ClientEnv mgr baseUrl) login) $ take nThreads logins
                  atomically $ writeTChan logChan Done
                  return results
              )
  dispResults res

runSPINIntake :: Appian (Maybe Text) -> FilePath -> Int -> BaseUrl -> FilePath -> IO ()
runSPINIntake script userFile nThreads baseUrl fp = do
  stderrLn $ intercalate " " [pack userFile, tshow baseUrl, pack fp, tshow nThreads]
  stderrLn "\n********************************************************************************\n"
  stderrLn "Waiting for 2 mins. Does the above look correct?"
  threadDelay (120000000)
  stderrLn "Starting SPIN Change intake now!"
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  chan <- newTChanIO
  (_, _) <- concurrently (concurrently
                            (loggingFunc fp)
                            (loginProducer userFile chan)
                           )
              (do
                  atomically $ writeTChan logChan $ Msg "timeStamp,elapsed,label,responseCode"
                  results <- mapConcurrently (flip loginConsumer (runIt mgr)) $ take nThreads $ repeat chan
                  atomically $ writeTChan logChan Done
                  return results
              )
  putStrLn "Finished with SPIN Change Intake!"
    where
      runIt mgr login = do
        res <- tryAny $ runAppian script (ClientEnv mgr baseUrl) login
        print res

runMultiple :: (FilePath -> Int -> IO ()) -> FilePath -> Int -> [Int] -> IO ()
runMultiple script logFilePrefix nRuns nUserList = mapM_ (mapM_ runScript . zip [1..nRuns] . repeat) nUserList
  where
    runScript (run, nUsers) = script logFp nUsers
      where
        logFp = logFilePrefix <> suffix
        suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

runInitialReview :: BaseUrl -> String -> FilePath -> Int -> IO ()
runInitialReview baseUrl username fp nThreads = do
  stderrLn $ intercalate " " [tshow baseUrl, pack fp, tshow nThreads]
  stderrLn "\n********************************************************************************\n"
  stderrLn "Waiting for 2 mins. Does the above look correct?"
  threadDelay (120000000)
  stderrLn "Starting SPIN Change initial review now!"
  password <- getPassword
  conf <- newReviewConf
  let actions = take nThreads $ repeat (initialReview conf adminInitial2017)
      login = Login (pack username) $ pack password
  atomically $ writeTChan logChan $ Msg "timeStamp,elapsed,label,responseCode"
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)

  results <- runConcurrently
    $  Concurrently (loggingFunc fp)
    *> Concurrently (mapConcurrently (\f -> tryAny $ runAppian f (ClientEnv mgr baseUrl) login) actions)
  atomically $ writeTChan logChan Done

  dispResults results

runReview :: BaseUrl -> FilePath -> Int -> IO ()
runReview baseUrl fp nThreads = do
  stderrLn $ "Running review on " <> tshow baseUrl <> " for " <> tshow nThreads <> " threads."
  mgr <- newManager tlsManagerSettings
  results <- runConcurrently
    $  Concurrently (loggingFunc fp)
    *> Concurrently (mapConcurrently (const $ dispatchReview RevSpinChange (ClientEnv mgr baseUrl)) [1..nThreads])
  atomically $ writeTChan logChan Done

  dispResults results

runAdminIntake :: FilePath -> Int -> BaseUrl -> FilePath -> IO ()
runAdminIntake userFile nThreads baseUrl fp = runConsumer action baseUrl fp userFile nThreads
  where
    action login = adminIntake $ login ^. username . to AppianUsername
  
runConsumer :: (Login -> Appian a) -> BaseUrl -> FilePath -> FilePath -> Int -> IO ()
runConsumer f baseUrl fp userFile nThreads = do
  chan <- atomically newTChan
  mgr <- liftIO $ newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  _ <- concurrently (concurrently
                      (loggingFunc fp)
                      (loginProducer userFile chan)
                    )
           (do
               atomically $ writeTChan logChan $ Msg "timeStamp,elapsed,label,responseCode"
               results <- mapConcurrently (flip loginConsumer (runIt mgr)) $ take nThreads $ repeat chan
               atomically $ writeTChan logChan Done
               return results
           )
  putStrLn "Done!"
    where
      runIt mgr login = do
        res <- join <$> (tryAny $ runAppian (f login) (ClientEnv mgr baseUrl) login)
        case res of
          Left exc -> print exc
          Right _ -> putStrLn "Success!"

newtype UnsupportedReviewException = UnsupportedReviewException Text

instance Show UnsupportedReviewException where
  show (UnsupportedReviewException msg) = "UnsupportedReviewException: " <> unpack msg

instance Exception UnsupportedReviewException

dispatchReview :: ReviewType -> ClientEnv -> IO (Either SomeException Value)
dispatchReview RevSpinChange env = spinReview env preprodSpinReviewUsers
dispatchReview RevForm486 env = form486Review env preprodForm486Users
dispatchReview RevServSub env = servSubReview env preprodServSubUsers
dispatchReview typ _ = throwM $ UnsupportedReviewException $ tshow typ

loginConsumer :: MonadIO m => TChan (ThreadControl a) -> (a -> m ()) -> m ()
loginConsumer chan f = S.mapM_ f $ S.map tcItem $ S.takeWhile notFinished $ S.repeatM (atomically $ readTChan chan)

loginProducer :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => FilePath -> TChan (ThreadControl Login) -> m ()
loginProducer fp chan = do
  csvStreamByName >>> S.map Item >>> S.mapM_ (atomically . writeTChan chan) >>> runResourceT >>> runNoLoggingT $ fp
  atomically $ writeTChan chan Finished

run486Intake :: BaseUrl -> FilePath -> Int -> [Int] -> FilePath -> IO ()
run486Intake baseUrl fpPrefix nRuns nUserList logFilePrefix = mapM_ (mapM_ run486Intake . zip [1..nRuns] . repeat) nUserList
  where
    run486Intake (run, nUsers) = runScript (Form486 form486Intake userFp) baseUrl mempty logFp nUsers
      where
        userFp = fpPrefix <> suffix
        logFp = logFilePrefix <> suffix
        suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

setTimeout :: ResponseTimeout -> ManagerSettings -> ManagerSettings
setTimeout timeout settings = settings { managerResponseTimeout = timeout }

dispResults :: [Either SomeException a] -> IO ()
dispResults results = do
  let appianErrors = results ^.. traverse . _Left
      successes = results ^.. traverse . _Right
  mapM_ print appianErrors
  putStrLn $ "Successfully executed: " <> tshow (length successes)
  putStrLn $ "Appian Errors: " <> tshow (length appianErrors)

commandsInfo :: ParserInfo (IO ())
commandsInfo = info (helper <*> parseCommands)
  (  fullDesc
  <> progDesc "Various scripts written for the EPC system."
  )

parseCommands :: Parser (IO ())
parseCommands = subparser
  (  command "scripts" scriptsInfo
  <> command "form486Intake" form486Info
  <> command "spinChangeIntake" spinChangeInfo
  <> command "initialReview" initialReviewInfo
  <> command "review" reviewInfo
  <> command "adminIntake" adminIntakeInfo
  )

scriptsInfo :: ParserInfo (IO ())
scriptsInfo = info (helper <*> scriptsParser)
  (  fullDesc
  <> progDesc "Various scripts written for the EPC system."
  )

scriptsParser :: Parser (IO ())
scriptsParser = runScript
  <$> option scriptParser
  (  long "run-script"
  <> short 'r'
  )
  <*> urlParser
  <*> strOption
  (  long "username"
  <> short 'u'
  )
  <*> strOption
  (  long "log-file-path"
  <> short 'l'
  <> help "The path of the file to write the logs to."
  )
  <*> option auto
  (  long "nThreads"
  <> short 'n'
  <> help "The number of threads to execute with."
  )

urlParser :: Parser BaseUrl
urlParser = BaseUrl
  <$> flag Http Https
  (  long "secure"
  <> short 's'
  <> help "If set uses https."
  )
  <*> strOption
  (  long "host-name"
  <> short 'n'
  <> help "The hostname of the server to use."
  )
  <*> option auto
  (  long "port"
  <> short 'p'
  <> help "The port to use for connecting to the remote host."
  )
  <*> pure ""

data Script
  = CSScript (Appian Text)
  | Form471 (Appian Value)
  | Form486 (AppianUsername -> Appian (Maybe Text)) FilePath
  | SPINChange (Appian Value) FilePath

scriptParser :: ReadM Script
scriptParser = do
  name <- readerAsk
  case name of
    "cscase" -> return $ CSScript createCSCase
    "form471" -> return $ Form471 (form471Intake "143000618")
    _ -> fail $ show name <> " is not a valid script name!"

form486Info :: ParserInfo (IO ())
form486Info = info (helper <*> form486Parser)
  (  fullDesc
  <> progDesc "Runs the FCC Form 486 intake performance script"
  )

form486Parser :: Parser (IO ())
form486Parser = run486Intake
  <$> urlParser
  <*> userParser
  <*> option auto
  (  long "num-runs"
  <> short 'n'
  )
  <*> threadsParser
  <*> logFileParser

spinChangeInfo :: ParserInfo (IO ())
spinChangeInfo = info (helper <*> spinChangeParser)
  (  fullDesc
  <> progDesc "Runs the SPIN Change intake script."
  )

spinChangeParser :: Parser (IO ())
spinChangeParser = runSPINIntake spinChangeIntake
  <$> userParser
  <*> option auto
  (  long "num-threads"
  <> short 't'
  )
  <*> urlParser
  <*> logFileParser

initialReviewInfo :: ParserInfo (IO ())
initialReviewInfo = info (helper <*> initialReviewParser)
  (  fullDesc
  <> progDesc "Runs the Initial Review script."
  )

initialReviewParser :: Parser (IO ())
initialReviewParser = runMultiple
  <$> (runInitialReview
   <$> urlParser
   <*> userParser
  )
  <*> logFileParser
  <*> option auto
  (  long "num-runs"
  <> short 'n'
  )
  <*> threadsParser

reviewInfo :: ParserInfo (IO ())
reviewInfo = info (helper <*> reviewParser)
  (  fullDesc
  <> progDesc "Runs PC review for 486, Service Substitution, SPIN Change, and Appeals."
  )

reviewParser :: Parser (IO ())
reviewParser = runReview
  <$> urlParser
  <*> logFileParser
  <*> option auto
  (  long "num-threads"
  <> short 't'
  )

adminIntakeInfo :: ParserInfo (IO ())
adminIntakeInfo = info (helper <*> adminIntakeParser)
  (  fullDesc
  <> progDesc "Runs Administrative Correction intake"
  )

adminIntakeParser :: Parser (IO ())
adminIntakeParser = runAdminIntake
  <$> userParser
  <*> option auto
  (  long "num-threads"
  <> short 't'
  )
  <*> urlParser
  <*> logFileParser

parseMany :: ReadM [String]
parseMany = readerAsk >>= pure . words
  
parseManyR :: Read a => ReadM [a]
parseManyR = parseMany >>= readMany
  where
    readMany l = traverse readIt l
    readIt x = case readMay x of
      Just y -> return y
      Nothing -> fail $ "Could not read " <> show x

logFileParser :: Parser String
logFileParser = strOption
  (  long "log-file-path"
  <> short 'l'
  <> help "The path of the file to write the logs to."
  )

userParser :: Parser FilePath
userParser = strOption
  (  long "user-csv-prefix"
  <> short 'i'
  )

threadsParser :: Parser [Int]
threadsParser = option parseManyR
  (  long "num-users-list"
  <> short 'u'
  )

stderrLn :: MonadIO m => Text -> m ()
stderrLn txt = hPut stderr $ encodeUtf8 txt <> "\n"
