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
import Scripts.ComadReview
import Appian.Client ( runAppianT, cookieModifier, LogFilePath, logFilePath, runAppianT'
                     , LogMode (..), HostUrl (..), ScriptError (..), Appian, _BadUpdateError
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
import Control.Monad.Except (catchError, throwError)
import Scripts.Noise

getPassword :: IO String
getPassword = pure "EPCPassword123!"

runMultiple :: (LogMode -> Int -> IO ()) -> LogFilePath -> Int -> [Int] -> IO ()
runMultiple script logFilePrefix nRuns nUserList = mapM_ (mapM_ runScript . zip [1..nRuns] . repeat) nUserList
  where
    runScript (run, nUsers) = script (LogFile logFp) nUsers
      where
        logFp = logFilePrefix <> logFilePath suffix
        suffix = "_" <> show nUsers <> "_" <> show run <> ".csv"

runAdminIntake :: FilePath -> Int -> BaseUrl -> LogMode -> IO ()
runAdminIntake = error "Appeal intake has been broken!"
-- runAdminIntake userFile nThreads baseUrl fp = runConsumer action baseUrl fp userFile nThreads
--   where
--     action login = adminIntake $ login ^. username . to AppianUsername
  
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

run471Intake :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError Form471Num)]
run471Intake = runIt form471Intake

run471IntakeAndCertify :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError Form471Num)]
run471IntakeAndCertify = runIt form471IntakeAndCertify

run486Intake :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError (Maybe Text))]
run486Intake = runIt form486Intake

runInitialReview :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError Value)]
runInitialReview conf = runIt (initialReview conf)

runReviewAssign :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> Int -> IO [Either ServantError (Either ScriptError Value)]
runReviewAssign conf = runScriptExhaustive (assignment conf)

runNoise :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError ())]
runNoise = runIt noise

run471Review :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError Value)]
run471Review = runIt form471Review

shouldRetry :: Monad m => RetryStatus -> Either ScriptError a -> m Bool
shouldRetry _ (Left err) = case err ^? _BadUpdateError . _1 of -- to fromException . traverse . badUpdateExceptionMsg of
    Nothing -> pure False
    Just txt -> pure $ isInfixOf "Cannot find task for " txt
shouldRetry _ (Right _) = pure False

findTaskRetryPolicy :: Monad m => RetryPolicyM m
findTaskRetryPolicy = exponentialBackoff 1000000 `mappend` limitRetries 10

form471IntakeAndCertify :: Form471Conf -> Appian Form471Num
form471IntakeAndCertify conf = do
    formNum <- form471Intake conf
    let certConf = CertConf formNum $ conf ^. applicant
        certify = do
          res <- form471Certification certConf
          return $ Right res
        certifyCatch = return . Left
    eRes <- retrying findTaskRetryPolicy shouldRetry (const $ (certify `catchError` certifyCatch))
    either throwError pure eRes

runComadInitialReview :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError Value)]
runComadInitialReview baseConf = runIt $ comadInitialReview baseConf

runReview :: ReviewBaseConf -> Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError Value)]
runReview baseConf = runIt (finalReview baseConf)

runIt :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError b)]
runIt f bounds (HostUrl hostUrl) logMode csvInput (RampupTime delay) (NThreads n) = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState bounds

  res <- runResourceT $ runStderrLoggingT $ runParallel $ Parallel (nThreads n) (S.zip (S.each [0..]) $ void (csvStreamByName csvInput)) (\(i, a) -> do
                                                                                                               let d = (i * (delay `div` n))
                                                                                                               threadDelay $ trace (show d) d
                                                                                                               res <- liftIO $ runAppianT logMode (f a) appianState env (getLogin a)
                                                                                                               return res
                                                                                                           )
  let justs = fmap fromJust . filter isJust
      fromJust (Just v) = v
      res' = justs res
  dispResults res'
  return res'

exhaustiveProducer :: (Csv.FromNamedRecord a, MonadResource m, MonadLogger m) => TBQueue a -> CsvPath -> m String
exhaustiveProducer q = csvStreamByName >>> S.mapM_ (atomically . writeTBQueue q)

runScriptExhaustive :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> Int -> IO [Either ServantError (Either ScriptError b)]
runScriptExhaustive f bounds (HostUrl hostUrl) logMode csvInput nThreads numRecords = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState bounds
  confs <- csvStreamByName >>> S.take numRecords >>> S.toList >>> runResourceT >>> runStdoutLoggingT $ csvInput
  res <- execTaskGroup nThreads (\a -> runAppianT logMode (f a) appianState env (getLogin a)) $ S.fst' confs
  dispResults res
  return res

execTaskGroup :: Traversable t => NThreads -> (a -> IO b) -> t a -> IO (t b)
execTaskGroup (NThreads n) f args = Pool.withTaskGroup n $ \group -> Pool.mapConcurrently group f args

execTaskGroup_ :: Traversable t => NThreads -> (a -> IO b) -> t a -> IO ()
execTaskGroup_ n f args = execTaskGroup n f args >> return ()

runSPINIntake :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Either ServantError (Either ScriptError (Maybe Text))]
runSPINIntake = runIt spinChangeIntake

setTimeout :: ResponseTimeout -> ManagerSettings -> ManagerSettings
setTimeout timeout settings = settings { managerResponseTimeout = timeout }

dispResults :: [Either ServantError (Either ScriptError a)] -> IO ()
dispResults results = do
  let appianErrors = results ^.. traverse . _Right . _Left
      serverErrors = results ^.. traverse . _Left
      successes = results ^.. traverse . _Right . _Right
  mapM_ print serverErrors
  mapM_ print appianErrors
  putStrLn $ "Successfully executed: " <> tshow (length successes)
  putStrLn $ "Script Errors: " <> tshow (length appianErrors)
  putStrLn $ "Server Errors: " <> tshow (length serverErrors)

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
  <> command "form471Review" form471ReviewInfo
  <> command "noise" noiseInfo
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

hostUrlParser :: Parser HostUrl
hostUrlParser = HostUrl
  <$> strOption
  (  long "host-name"
  <> short 'n'
  <> help "The hostname of the server to use."
  )

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
  <*> rampupParser
  <*> nthreadParser

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
  <*> rampupParser
  <*> nthreadParser

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
  <*> rampupParser
  <*> nthreadParser

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
  <*> rampupParser
  <*> nthreadParser

form471ReviewInfo :: ParserInfo (IO ())
form471ReviewInfo = info (helper <*> form471ReviewParser)
  (  fullDesc
  <> progDesc "Runs the 2017 SPIN Change Initial Review script"
  )

form471ReviewParser :: Parser (IO ())
form471ReviewParser = fmap void $ run471Review
  <$> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> rampupParser
  <*> nthreadParser

noiseInfo :: ParserInfo (IO ())
noiseInfo = info (helper <*> noiseParser)
  (  fullDesc
  <> progDesc "Runs the 2017 SPIN Change Initial Review script"
  )

noiseParser :: Parser (IO ())
noiseParser = fmap void $ runNoise
  <$> boundsParser
  <*> hostUrlParser
  <*> logModeParser
  <*> csvConfParser
  <*> rampupParser
  <*> nthreadParser

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
  <*> nthreadParser
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
  <*> rampupParser
  <*> nthreadParser

csvConfParser :: Parser CsvPath
csvConfParser = fromString <$>
  strOption
  (  long "csv-conf"
  <> short 'i'
  <> help "The csv config file for 471 intake."
  )

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
  <*> rampupParser
  <*> nthreadParser
                            )

nthreadParser :: Parser NThreads
nthreadParser = NThreads
  <$> option auto
  (  long "nThreads"
  <> help "The number of concurrent threads to execute."
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

newtype RampupTime = RampupTime Int
  deriving (Show, Eq, Num)

mkRampup :: Int -> RampupTime
mkRampup n = RampupTime $ n * 1000000

rampupParser :: Parser RampupTime
rampupParser = mkRampup
  <$> option auto
     (  long "rampup"
     <> help "The rampup period (in seconds) for the script"
     )
