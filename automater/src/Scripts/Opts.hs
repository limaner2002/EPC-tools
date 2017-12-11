{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scripts.Opts
  ( commandsInfo
  , setTimeout
  , module Scripts.Opts
  ) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types

import Scripts.FCCForm471
import Scripts.FCCForm471ReviewAssignment
import Scripts.FCCForm471Certification
import Scripts.FCCForm471Common (Form471Num)
import Scripts.CreateCSCase
import Scripts.FCCForm486
import Scripts.SPINChangeIntake
import Scripts.InitialReview
import Scripts.Assignment
import Scripts.ReviewCommon
import Scripts.AdminReview
import Scripts.AdminIntake
import Scripts.ComadReview
import Appian.Client ( runAppianT, cookieModifier, LogFilePath, logFilePath, runAppianT'
                     , LogMode (..), HostUrl (..), MissingComponentException (..), badUpdateExceptionMsg
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
import Control.Retry
import qualified Control.Concurrent.Async.Pool as Pool

getPassword :: IO String
getPassword = pure "EPCPassword123!"

-- runScript :: Script -> BaseUrl -> String -> LogMode -> Int -> IO ()
-- runScript (CSScript script) baseUrl username fp nThreads = do
--   mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
--   password <- getPassword
--   let login = Login (pack username) (pack password)

--   results <- mapConcurrently (const $ tryAny $ runAppianT fp script (ClientEnv mgr baseUrl) login) $ [1..nThreads]

--   dispResults results

-- runScript (Form486 script userFile) baseUrl _ fp nThreads = do
--   stderrLn $ intercalate " " [tshow userFile, tshow baseUrl, tshow fp, tshow nThreads]
--   stderrLn "\n********************************************************************************\n"
--   stderrLn "Waiting for 2 mins. Does the above look correct?"
--   threadDelay (120000000)
--   stderrLn "Starting 486 intake now!"
--   mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
--   logins <- S.fst' <$> (csvStreamByName >>> S.toList >>> runResourceT >>> runNoLoggingT $ userFile)

--   results <- mapConcurrently (\login -> tryAny $ runAppianT fp (script $ login ^. username . to AppianUsername) (ClientEnv mgr baseUrl) login) $ take nThreads logins

--   dispResults results

runMultiple :: (LogMode -> Int -> IO ()) -> LogFilePath -> Int -> [Int] -> IO ()
runMultiple script logFilePrefix nRuns nUserList = mapM_ (mapM_ runScript . zip [1..nRuns] . repeat) nUserList
  where
    runScript (run, nUsers) = script (LogFile logFp) nUsers
      where
        logFp = logFilePrefix <> logFilePath suffix
        suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

-- runInitialReview :: BaseUrl -> String -> LogMode -> Int -> IO ()
-- runInitialReview = error "Initial review is broken due to the rewrite such that review stages take the case number as inputs now."
-- runInitialReview baseUrl username fp nThreads = do
--   stderrLn $ intercalate " " [tshow baseUrl, tshow fp, tshow nThreads]
--   stderrLn "\n********************************************************************************\n"
--   stderrLn "Waiting for 2 mins. Does the above look correct?"
--   threadDelay (120000000)
--   stderrLn "Starting SPIN Change initial review now!"
--   password <- getPassword
--   conf <- newReviewConf
--   let actions = take nThreads $ repeat (initialReview conf adminInitial2017)
--       login = Login (pack username) $ pack password
--   mgr <- newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)

--   results <- mapConcurrently (\f -> tryAny $ runAppianT fp f (ClientEnv mgr baseUrl) login) actions

--   dispResults results

runAdminIntake :: FilePath -> Int -> BaseUrl -> LogMode -> IO ()
runAdminIntake = error "Appeal intake has been broken!"
-- runAdminIntake userFile nThreads baseUrl fp = runConsumer action baseUrl fp userFile nThreads
--   where
--     action login = adminIntake $ login ^. username . to AppianUsername
  
runConsumer :: (Login -> Appian a) -> BaseUrl -> LogMode -> FilePath -> Int -> IO ()
runConsumer f baseUrl fp userFile nThreads = do
  chan <- atomically newTChan
  mgr <- liftIO $ newManager (setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings)
  results <- mapConcurrently (flip loginConsumer (runIt mgr)) $ take nThreads $ repeat chan
  putStrLn "Done!"
    where
      runIt mgr login = do
        let appianState = newAppianState (Bounds 0 0)
        res <- join <$> (tryAny $ runAppianT fp (f login) appianState (ClientEnv mgr baseUrl) login)
        case res of
          Left exc -> print exc
          Right _ -> putStrLn "Success!"

newtype UnsupportedReviewException = UnsupportedReviewException Text

instance Show UnsupportedReviewException where
  show (UnsupportedReviewException msg) = "UnsupportedReviewException: " <> unpack msg

instance Exception UnsupportedReviewException

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

run471Intake :: Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException Form471Num]
run471Intake = runIt form471Intake

run471IntakeAndCertify :: Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException Form471Num]
run471IntakeAndCertify = runIt form471IntakeAndCertify

run486Intake :: Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException (Maybe Text)]
run486Intake = runIt form486Intake

runInitialReview :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException Value]
runInitialReview conf = runIt (initialReview conf)

runReviewAssign :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> Int -> IO [Either SomeException Value]
runReviewAssign conf = runScriptExhaustive (assignment conf)

run471Assign :: BaseUrl -> LogMode -> CsvPath -> Int -> IO ()
run471Assign baseUrl logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr baseUrl
      appianState = newAppianState (Bounds 0 0)

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\conf -> fmap join $ tryAny $ liftIO $ runAppianT logFilePath (form471Assign conf) appianState env (conf ^. confReviewMgr))
  dispResults $ fmap (maybe (throwM MissingItemException) id) res

run471Review :: String -> LogMode -> CsvPath -> Int -> IO ()
run471Review hostUrl logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (f env)
  dispResults $ fmap (maybe (throwM MissingItemException) id) res
  where
    f env conf = do
      let appianState = newAppianState (Bounds 0 0)
      eRes <- fmap join $ tryAny $ liftIO $ runAppianT logFilePath (form471Review conf) appianState env (conf ^. confReviewer)
      case eRes of
        Left exc -> return $ Left $ toException $ ScriptException $ (conf ^. confFormNum . to tshow) <> ": " <> tshow exc <> "\n" <> (conf ^. confReviewer . username . to tshow)
        Right _ -> return eRes
  
run471Certify :: HostUrl -> LogMode -> CsvPath -> Int -> IO ()
run471Certify (HostUrl hostUrl) logFilePath csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState (Bounds 0 0)

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\conf -> fmap join $ tryAny $ liftIO $ runAppianT' runStderrLoggingT (form471Certification conf) appianState env (conf ^. certLogin))
  dispResults $ fmap (maybe (throwM MissingItemException) id) res

-- run486Intake :: BaseUrl -> FilePath -> Int -> [Int] -> LogFilePath -> IO ()
-- run486Intake baseUrl fpPrefix nRuns nUserList logFilePrefix = mapM_ (mapM_ run486Intake . zip [1..nRuns] . repeat) nUserList
--   where
--     run486Intake (run, nUsers) = runScript (Form486 form486Intake $ fromString userFp) baseUrl mempty (LogFile logFp) nUsers
--       where
--         userFp = fpPrefix <> suffix
--         logFp = logFilePrefix <> logFilePath suffix
--         suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

shouldRetry :: Monad m => RetryStatus -> Either SomeException a -> m Bool
shouldRetry _ (Left exc) = case exc ^? to fromException . traverse . badUpdateExceptionMsg of
    Nothing -> pure False
    Just txt -> pure $ isPrefixOf "Cannot find task for " txt
    where
        fromMissing (MissingComponentException tpl) = tpl
shouldRetry _ (Right _) = pure False

findTaskRetryPolicy :: Monad m => RetryPolicyM m
findTaskRetryPolicy = exponentialBackoff 1000000 `mappend` limitRetries 10

form471IntakeAndCertify :: Form471Conf -> Appian Form471Num
form471IntakeAndCertify conf = do
    formNum <- form471Intake conf
    let certConf = CertConf formNum $ conf ^. applicant
    eRes <- retrying findTaskRetryPolicy shouldRetry (const $ tryAny $ form471Certification certConf)
    either throwM pure eRes

runComadInitialReview :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException Value]
runComadInitialReview baseConf = runIt $ comadInitialReview baseConf

runReview :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException Value]
runReview baseConf = runIt (finalReview baseConf)

runIt :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException b]
runIt f bounds (HostUrl hostUrl) logMode csvInput n = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState bounds

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (csvStreamByName csvInput) (\a -> do
                                                                                                               res <- fmap join $ tryAny $ liftIO $ runAppianT logMode (f a) appianState env (getLogin a)
                                                                                                               return res
                                                                                                           )
  let res' = fmap (maybe (throwM MissingItemException) id) res
  dispResults res'
  return res'

exhaustiveProducer :: (Csv.FromNamedRecord a, MonadResource m, MonadLogger m) => TBQueue a -> CsvPath -> m String
exhaustiveProducer q = csvStreamByName >>> S.mapM_ (atomically . writeTBQueue q)

runScriptExhaustive :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> Int -> IO [Either SomeException b]
runScriptExhaustive f bounds (HostUrl hostUrl) logMode csvInput nThreads numRecords = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState bounds
  confs <- csvStreamByName >>> S.take numRecords >>> S.toList >>> runResourceT >>> runStdoutLoggingT $ csvInput
  res <- execTaskGroup nThreads (\a -> fmap join $ tryAny $ runAppianT logMode (f a) appianState env (getLogin a)) $ S.fst' confs
  dispResults res
  return res

execTaskGroup :: Traversable t => NThreads -> (a -> IO b) -> t a -> IO (t b)
execTaskGroup (NThreads n) f args = Pool.withTaskGroup n $ \group -> Pool.mapConcurrently group f args

execTaskGroup_ :: Traversable t => NThreads -> (a -> IO b) -> t a -> IO ()
execTaskGroup_ n f args = execTaskGroup n f args >> return ()

runSPINIntake :: Bounds -> HostUrl -> LogMode -> CsvPath -> Int -> IO [Either SomeException (Maybe Text)]
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
  <> command "comadInitial" comadInitialInfo
  <> command "form471IntakeAndCertify" form471IntakeAndCertifyInfo
  <> command "form486Intake" form486IntakeInfo
  <> command "initialReview" initialReviewInfo
  <> command "pcAssign" reviewAssignInfo
  -- <> command "scripts" scriptsInfo
  -- <> command "form486Intake" form486Info
  -- <> command "spinChangeIntake" spinChangeInfo
  -- <> command "initialReview" initialReviewInfo
  -- <> command "review" reviewInfo
  -- <> command "adminIntake" adminIntakeInfo
  )

-- scriptsInfo :: ParserInfo (IO ())
-- scriptsInfo = info (helper <*> scriptsParser)
--   (  fullDesc
--   <> progDesc "Various scripts written for the EPC system."
--   )

-- scriptsParser :: Parser (IO ())
-- scriptsParser = runScript
--   <$> option scriptParser
--   (  long "run-script"
--   <> short 'r'
--   )
--   <*> urlParser
--   <*> strOption
--   (  long "username"
--   <> short 'u'
--   )
--   <*> logModeParser
--   -- <*> strOption
--   -- (  long "log-file-path"
--   -- <> short 'l'
--   -- <> help "The path of the file to write the logs to."
--   -- )
--   <*> option auto
--   (  long "nThreads"
--   <> short 'n'
--   <> help "The number of threads to execute with."
--   )

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

hostUrlParser :: Parser HostUrl
hostUrlParser = HostUrl
  <$> strOption
  (  long "host-name"
  <> short 'n'
  <> help "The hostname of the server to use."
  )

-- scriptParser :: ReadM Script
-- scriptParser = do
--   name <- readerAsk
--   let conf = Form471Conf {_nFRNs = 5, _spin = "143000413", _applicant = Login {_username = "mcmechd@gardencityschools.com", _password = "EPCPassword123!"}}
--   case name of
--     "cscase" -> return $ CSScript createCSCase
--     "form471" -> return $ Form471 (form471Intake conf)
--     _ -> fail $ show name <> " is not a valid script name!"

form471IntakeInfo :: ParserInfo (IO ())
form471IntakeInfo = info (helper <*> form471Parser)
  (  fullDesc
  <> progDesc "Runs the FCC Form 471 intake performance script"
  )

form471Parser :: Parser (IO ())
form471Parser = fmap void $ run471Intake
  <$> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

form471IntakeAndCertifyInfo :: ParserInfo (IO ())
form471IntakeAndCertifyInfo = info (helper <*> form471IntakeAndCertifyParser)
  (  fullDesc
  <> progDesc "Runs the 471 Intake Script followed by the 471 Certification script."
  )

form471IntakeAndCertifyParser :: Parser (IO ())
form471IntakeAndCertifyParser = fmap void $ run471IntakeAndCertify
  <$> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

form486IntakeInfo :: ParserInfo (IO ())
form486IntakeInfo = info (helper <*> form486IntakeParser)
  (  fullDesc
  <> progDesc "Runs the FCC Form 486 Intake script"
  )

form486IntakeParser :: Parser (IO ())
form486IntakeParser = fmap void $ run486Intake
  <$> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

initialReviewInfo :: ParserInfo (IO ())
initialReviewInfo = info (helper <*> initialReviewParser)
  (  fullDesc
  <> progDesc "Runs the 2017 SPIN Change Initial Review script"
  )

initialReviewParser :: Parser (IO ())
initialReviewParser = fmap void $ runInitialReview
  <$> reviewBaseConfParser
  <*> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  )

reviewAssignInfo :: ParserInfo (IO ())
reviewAssignInfo = info (helper <*> reviewAssignParser)
  (  fullDesc
  <> progDesc "Runs the 2017 SPIN Change Initial Review script"
  )

reviewAssignParser :: Parser (IO ())
reviewAssignParser = fmap void $ runReviewAssign
  <$> reviewBaseConfParser
  <*> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> (NThreads <$> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  ))
  <*> option auto
  (  long "numRecords"
  <> help "The total number of records to exhaust."
  )

comadInitialInfo :: ParserInfo (IO ())
comadInitialInfo = info (helper <*> comadInitialParser)
  (  fullDesc
  <> progDesc "Runs the PC COMAD Initial Review script"
  )

comadInitialParser :: Parser (IO ())
comadInitialParser = fmap void $ runComadInitialReview
  <$> pure comadInitial2017
  <*> boundsParser
  <*> hostUrlParser
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

-- form486Info :: ParserInfo (IO ())
-- form486Info = info (helper <*> form486Parser)
--   (  fullDesc
--   <> progDesc "Runs the FCC Form 486 intake performance script"
--   )

-- form486Parser :: Parser (IO ())
-- form486Parser = run486Intake
--   <$> urlParser
--   <*> userParser
--   <*> option auto
--   (  long "num-runs"
--   <> short 'n'
--   )
--   <*> threadsParser
--   <*> logFileParser

spinChangeInfo :: ParserInfo (IO ())
spinChangeInfo = info (helper <*> spinChangeParser)
  (  fullDesc
  <> progDesc "Runs the SPIN Change intake script."
  )

spinChangeParser :: Parser (IO ())
spinChangeParser = void <$> (runSPINIntake
  <$> boundsParser
  <*> (HostUrl <$> strOption
  (  long "host-url"
  <> help "The url of the host to use."
  ))
  <*> logModeParser
  <*> csvConfParser
  <*> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
  ))

-- initialReviewInfo :: ParserInfo (IO ())
-- initialReviewInfo = info (helper <*> initialReviewParser)
--   (  fullDesc
--   <> progDesc "Runs the Initial Review script."
--   )

-- initialReviewParser :: Parser (IO ())
-- initialReviewParser = runMultiple
--   <$> (runInitialReview
--    <$> urlParser
--    <*> userParser
--   )
--   <*> logFileParser
--   <*> option auto
--   (  long "num-runs"
--   <> short 'n'
--   )
--   <*> threadsParser

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

boundsParser :: Parser Bounds
boundsParser = Bounds
  <$> option auto
  (  long "lower"
  <> help "The minimum for the think timer"
  )
  <*> option auto
  (  long "upper"
  <> help "The maximum for the think timer"
  )

fyParser :: Parser FundingYear
fyParser = option readFy (long "fy")

readFy :: ReadM FundingYear
readFy = do
  str <- readerAsk
  readFy_ str

readFy_ "fy16" = pure FY2016
readFy_ "fy17" = pure FY2017
readFy_ "fy18" = pure FY2018
readFy_ _ = fail $ "Unrecognized fy! Valid ones are: " <> "fy16, fy17, and fy18"

reviewTypeParser :: Parser ReviewType
reviewTypeParser = option readReviewType (long "review-type")

readReviewType :: ReadM ReviewType
readReviewType = do
  str <- readerAsk
  readReviewType_ str

readReviewType_ "SPINChange" = pure RevSpinChange
readReviewType_ "appeal" =  pure RevAppeals
readReviewType_ "Form486" =  pure RevForm486
readReviewType_ "COMAD" =  pure RevCOMAD
readReviewType_ "Form500" =  pure RevForm500
readReviewType_ "ServSub" =  pure RevServSub
readReviewType_ "AdminCorrection" =  pure RevAdminCorrection
readReviewType_ "SRCSSPINChange" =  pure RevSRCSpinChange
readReviewType_ "BulkSPINChange" =  pure RevBulkSpinChange
readReviewType_ _ = fail $ "Unrecognized review type! Valid ones are: " <> "SPINChange, appeal, Form486, COMAD, ServSub, AdminCorrection, SRCSPINChange, and BulkSPINChange"

reviewerTypeParser :: Parser ReviewerType
reviewerTypeParser = option readReviewerType (long "reviewer-type")

readReviewerType :: ReadM ReviewerType
readReviewerType = do
  str <- readerAsk
  readReviewerType_ str

readReviewerType_ "initial" = pure RevInitial
readReviewerType_ "final" = pure RevInitial
readReviewerType_ "solix" = pure RevInitial
readReviewerType_ "usac" = pure RevInitial
readReviewerType_ "HSInit" = pure RevInitial
readReviewerType_ "HSFinal" = pure RevInitial
readReviewerType_ _ = fail $ "Unrecognized reviewer type! Valit ones are: " <> "initial, final, solix, usac, HSInitial, and HSFinal"

reviewBaseConfParser :: Parser ReviewBaseConf
reviewBaseConfParser = ReviewBaseConf
  <$> reviewTypeParser
  <*> reviewerTypeParser
  <*> fyParser

-- reviewTypeParser :: Parser ReviewType
-- reviewTypeParser =
--   (parseElement . pack)
--   <$>  
--   strOption
--   (  long "reviewType"
--   <> help "The type of case to review."
--   )

