{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scripts.Opts
  ( commandsInfo
  , runScript
  , Script (..)
  , setTimeout
  , module Scripts.Opts
  ) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types

import Scripts.FCCForm471
import Scripts.FCCForm471ReviewAssignment
import Scripts.FCCForm471Certification
import Scripts.CreateCSCase
import Scripts.FCCForm486
import Scripts.SPINChangeIntake
import Scripts.InitialReview
import Scripts.ReviewCommon
import Scripts.AdminReview
import Scripts.AdminIntake
import Appian.Client ( runAppianT, cookieModifier, LogFilePath, logFilePath, runAppianT'
                     , LogMode (..), HostUrl (..)
                     )
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
import Control.Monad.Trans.Resource (runResourceT)
import Control.Arrow ((>>>))
import qualified Data.Csv as Csv
import Scripts.ProducerConsumer

getPassword :: IO String
getPassword = pure "EPCPassword123!"

runScript :: Script -> BaseUrl -> String -> LogMode -> Int -> IO ()
runScript (CSScript script) baseUrl username fp nThreads = do
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  password <- getPassword
  let login = Login (pack username) (pack password)

  results <- mapConcurrently (const $ tryAny $ runAppianT fp script (ClientEnv mgr baseUrl) login) $ [1..nThreads]

  dispResults results
-- runScript (Form471 script) baseUrl username fp nThreads = do
--   mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
--   password <- getPassword
--   logins <- S.fst' <$> (csvStreamByName >>> S.drop 10 >>> S.toList >>> runResourceT >>> runNoLoggingT $ "applicantConsortiums.csv")
--   results <- mapConcurrently (\login -> tryAny $ runAppianT script (ClientEnv mgr baseUrl) login) $ take nThreads logins
--   dispResults results
runScript (Form486 script userFile) baseUrl _ fp nThreads = do
  stderrLn $ intercalate " " [tshow userFile, tshow baseUrl, tshow fp, tshow nThreads]
  stderrLn "\n********************************************************************************\n"
  stderrLn "Waiting for 2 mins. Does the above look correct?"
  threadDelay (120000000)
  stderrLn "Starting 486 intake now!"
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  logins <- S.fst' <$> (csvStreamByName >>> S.toList >>> runResourceT >>> runNoLoggingT $ userFile)

  results <- mapConcurrently (\login -> tryAny $ runAppianT fp (script $ login ^. username . to AppianUsername) (ClientEnv mgr baseUrl) login) $ take nThreads logins

  dispResults results

-- runSPINIntake :: Appian (Maybe Text) -> FilePath -> Int -> BaseUrl -> LogMode -> IO ()
-- runSPINIntake script userFile nThreads baseUrl fp = do
--   stderrLn $ intercalate " " [pack userFile, tshow baseUrl, tshow fp, tshow nThreads]
--   stderrLn "\n********************************************************************************\n"
--   stderrLn "Waiting for 2 mins. Does the above look correct?"
--   threadDelay (120000000)
--   stderrLn "Starting SPIN Change intake now!"
--   mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
--   chan <- newTChanIO

--   results <- mapConcurrently (flip loginConsumer (runIt mgr)) $ take nThreads $ repeat chan

--   putStrLn "Finished with SPIN Change Intake!"
--     where
--       runIt mgr login = do
--         res <- tryAny $ runAppianT (LogFile "/tmp/spinIntake.log") script (ClientEnv mgr baseUrl) login
--         print res

runMultiple :: (LogMode -> Int -> IO ()) -> LogFilePath -> Int -> [Int] -> IO ()
runMultiple script logFilePrefix nRuns nUserList = mapM_ (mapM_ runScript . zip [1..nRuns] . repeat) nUserList
  where
    runScript (run, nUsers) = script (LogFile logFp) nUsers
      where
        logFp = logFilePrefix <> logFilePath suffix
        suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

runInitialReview :: BaseUrl -> String -> LogMode -> Int -> IO ()
runInitialReview baseUrl username fp nThreads = do
  stderrLn $ intercalate " " [tshow baseUrl, tshow fp, tshow nThreads]
  stderrLn "\n********************************************************************************\n"
  stderrLn "Waiting for 2 mins. Does the above look correct?"
  threadDelay (120000000)
  stderrLn "Starting SPIN Change initial review now!"
  password <- getPassword
  conf <- newReviewConf
  let actions = take nThreads $ repeat (initialReview conf adminInitial2017)
      login = Login (pack username) $ pack password
  mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)

  results <- mapConcurrently (\f -> tryAny $ runAppianT fp f (ClientEnv mgr baseUrl) login) actions

  dispResults results

-- runReview :: BaseUrl -> FilePath -> Int -> IO ()
-- runReview baseUrl fp nThreads = do
--   stderrLn $ "Running review on " <> tshow baseUrl <> " for " <> tshow nThreads <> " threads."
--   mgr <- newManager tlsManagerSettings
--   results <- mapConcurrently (const $ dispatchReview RevSpinChange (ClientEnv mgr baseUrl)) [1..nThreads]

--   dispResults results

runAdminIntake :: FilePath -> Int -> BaseUrl -> LogMode -> IO ()
runAdminIntake userFile nThreads baseUrl fp = runConsumer action baseUrl fp userFile nThreads
  where
    action login = adminIntake $ login ^. username . to AppianUsername
  
runConsumer :: (Login -> Appian a) -> BaseUrl -> LogMode -> FilePath -> Int -> IO ()
runConsumer f baseUrl fp userFile nThreads = do
  chan <- atomically newTChan
  mgr <- liftIO $ newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  results <- mapConcurrently (flip loginConsumer (runIt mgr)) $ take nThreads $ repeat chan
  putStrLn "Done!"
    where
      runIt mgr login = do
        res <- join <$> (tryAny $ runAppianT fp (f login) (ClientEnv mgr baseUrl) login)
        case res of
          Left exc -> print exc
          Right _ -> putStrLn "Success!"

newtype UnsupportedReviewException = UnsupportedReviewException Text

instance Show UnsupportedReviewException where
  show (UnsupportedReviewException msg) = "UnsupportedReviewException: " <> unpack msg

instance Exception UnsupportedReviewException

-- dispatchReview :: ReviewType -> ClientEnv -> IO (Either SomeException Value)
-- dispatchReview RevSpinChange env = spinReview env preprodSpinReviewUsers
-- dispatchReview RevForm486 env = form486Review env preprodForm486Users
-- dispatchReview RevServSub env = servSubReview env preprodServSubUsers
-- dispatchReview typ _ = throwM $ UnsupportedReviewException $ tshow typ

loginConsumer :: MonadIO m => TChan (ThreadControl a) -> (a -> m ()) -> m ()
loginConsumer chan f = S.mapM_ f $ S.map tcItem $ S.takeWhile notFinished $ S.repeatM (atomically $ readTChan chan)

loginConsumer' :: MonadIO m => TChan (ThreadControl a) -> (a -> m b) -> m (S.Of [b] ())
loginConsumer' chan f = S.toList $ S.mapM f $ S.map tcItem $ S.takeWhile notFinished $ S.repeatM (atomically $ readTChan chan)

loginProducer :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, Csv.FromNamedRecord a) => CsvPath -> TChan (ThreadControl a) -> m ()
loginProducer fp chan = do
  csvStreamByName >>> S.map Item >>> S.mapM_ (atomically . writeTChan chan) >>> runResourceT >>> runNoLoggingT $ fp
  atomically $ writeTChan chan Finished

data MissingItemException = MissingItemException

instance Show MissingItemException where
  show _ = "There was nothing provided to this consumer!"

instance Exception MissingItemException

newtype ScriptException = ScriptException Text

instance Show ScriptException where
  show (ScriptException msg) = unpack msg

instance Exception ScriptException

run471Intake :: BaseUrl -> LogMode -> CsvPath -> Int -> IO ()
run471Intake baseUrl logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr baseUrl

  res <- runResourceT $ runNoLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\conf -> fmap join $ tryAny $ liftIO $ runAppianT' runStdoutLoggingT (form471Intake conf) env (conf ^. applicant))
  dispResults $ fmap (maybe (throwM MissingItemException) id) res

run471Assign :: BaseUrl -> LogMode -> CsvPath -> Int -> IO ()
run471Assign baseUrl logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr baseUrl

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\conf -> fmap join $ tryAny $ liftIO $ runAppianT logFilePath (form471Assign conf) env (conf ^. confReviewMgr))
  dispResults $ fmap (maybe (throwM MissingItemException) id) res

run471Review :: String -> LogMode -> CsvPath -> Int -> IO ()
run471Review hostUrl logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (f env)
  dispResults $ fmap (maybe (throwM MissingItemException) id) res
  where
    f env conf = do
      eRes <- fmap join $ tryAny $ liftIO $ runAppianT logFilePath (form471Review conf) env (conf ^. confReviewer)
      case eRes of
        Left exc -> return $ Left $ toException $ ScriptException $ (conf ^. confFormNum . to tshow) <> ": " <> tshow exc <> "\n" <> (conf ^. confReviewer . username . to tshow)
        Right _ -> return eRes
  
run471Certify :: HostUrl -> LogMode -> CsvPath -> Int -> IO ()
run471Certify (HostUrl hostUrl) logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\conf -> fmap join $ tryAny $ liftIO $ runAppianT' runStderrLoggingT (form471Certification conf) env (conf ^. certLogin))
  dispResults $ fmap (maybe (throwM MissingItemException) id) res

run486Intake :: BaseUrl -> FilePath -> Int -> [Int] -> LogFilePath -> IO ()
run486Intake baseUrl fpPrefix nRuns nUserList logFilePrefix = mapM_ (mapM_ run486Intake . zip [1..nRuns] . repeat) nUserList
  where
    run486Intake (run, nUsers) = runScript (Form486 form486Intake $ fromString userFp) baseUrl mempty (LogFile logFp) nUsers
      where
        userFp = fpPrefix <> suffix
        logFp = logFilePrefix <> logFilePath suffix
        suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

runIt :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> HostUrl -> LogMode -> CsvPath -> Int -> IO ()
runIt f (HostUrl hostUrl) logMode csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\a -> fmap join $ tryAny $ liftIO $ runAppianT logMode (f a) env (getLogin a))
  dispResults $ fmap (maybe (throwM MissingItemException) id) res

runSPINIntake :: HostUrl -> LogMode -> CsvPath -> Int -> IO ()
runSPINIntake = runIt spinChangeIntake

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
  (  command "form471Intake" form471IntakeInfo
  -- <> command "scripts" scriptsInfo
  -- <> command "form486Intake" form486Info
  -- <> command "spinChangeIntake" spinChangeInfo
  -- <> command "initialReview" initialReviewInfo
  -- <> command "review" reviewInfo
  -- <> command "adminIntake" adminIntakeInfo
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
  <*> logModeParser
  -- <*> strOption
  -- (  long "log-file-path"
  -- <> short 'l'
  -- <> help "The path of the file to write the logs to."
  -- )
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
  | Form486 (AppianUsername -> Appian (Maybe Text)) CsvPath
  | SPINChange (Appian Value) FilePath

scriptParser :: ReadM Script
scriptParser = do
  name <- readerAsk
  let conf = Form471Conf {_nFRNs = 5, _spin = "143000413", _applicant = Login {_username = "mcmechd@gardencityschools.com", _password = "EPCPassword123!"}}
  case name of
    "cscase" -> return $ CSScript createCSCase
    "form471" -> return $ Form471 (form471Intake conf)
    _ -> fail $ show name <> " is not a valid script name!"

form471IntakeInfo :: ParserInfo (IO ())
form471IntakeInfo = info (helper <*> form471Parser)
  (  fullDesc
  <> progDesc "Runs the FCC Form 471 intake performance script"
  )

form471Parser :: Parser (IO ())
form471Parser = run471Intake
  <$> urlParser
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

csvConfParser :: Parser CsvPath
csvConfParser = fromString <$>
  strOption
  (  long "csv-conf"
  <> short 'i'
  <> help "The csv config file for 471 intake."
  )

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
spinChangeParser = runSPINIntake
  <$> (HostUrl <$> strOption
  (  long "host-url"
  <> help "The url of the host to use."
  ))
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

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

-- reviewInfo :: ParserInfo (IO ())
-- reviewInfo = info (helper <*> reviewParser)
--   (  fullDesc
--   <> progDesc "Runs PC review for 486, Service Substitution, SPIN Change, and Appeals."
--   )

-- reviewParser :: Parser (IO ())
-- reviewParser = runReview
--   <$> urlParser
--   <*> logModeParser
--   <*> option auto
--   (  long "num-threads"
--   <> short 't'
--   )

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
  <*> logModeParser

parseMany :: ReadM [String]
parseMany = readerAsk >>= pure . words
  
parseManyR :: Read a => ReadM [a]
parseManyR = parseMany >>= readMany
  where
    readMany l = traverse readIt l
    readIt x = case readMay x of
      Just y -> return y
      Nothing -> fail $ "Could not read " <> show x

logModeParser :: Parser LogMode
logModeParser = (
  strOption
  (  long "stdout"
  <> help "Log messages to stdout."
  )) *> pure LogStdout
  <|>
  LogFile <$> logFileParser

logFileParser :: Parser LogFilePath
logFileParser = logFilePath <$> strOption
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
