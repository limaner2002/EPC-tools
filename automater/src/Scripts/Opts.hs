{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

module Scripts.Opts -- where
  ( commandsInfo
  , runScript
  , Script (..)
  ) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types

import Scripts.FCCForm471
import Scripts.CreateCSCase
import Scripts.FCCForm486
import Appian.Client (runAppian, LogMessage(..), logChan)
import Appian.Instances
import Appian.Types (AppianUsername (..))
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Arrow
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource
import Data.Aeson (Value)
import Control.Lens
import Network.HTTP.Client
import Appian
import Stats.CsvStream
import Control.Monad.Logger

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
  -- let login = Login (pack username) (pack password)
  --     logins = [login]
  (_, res) <- concurrently (loggingFunc fp)
              ( do
                  atomically $ writeTChan logChan $ Msg $ "timeStamp,elapsed,label,responseCode"
                  results <- mapConcurrently (\login -> tryAny $ runAppian script (ClientEnv mgr baseUrl) login) $ take nThreads logins
                  atomically $ writeTChan logChan Done
                  return results
              )
  dispResults res
runScript (Form486 script userFile) baseUrl _ fp nThreads = do
  putStrLn $ intercalate " " [pack userFile, tshow baseUrl, pack fp, tshow nThreads]
  putStrLn "\n********************************************************************************\n"
  putStrLn "Waiting for 2 mins. Does the above look correct?"
  threadDelay (120000000)
  putStrLn "Starting 486 intake now!"
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

dispResults :: [Either SomeException (Either ServantError a)] -> IO ()
dispResults results = do
  let appianErrors = results ^.. traverse . _Left
      clientErrors = results ^.. traverse . _Right . _Left
      successes = results ^.. traverse . _Right . _Right
  mapM_ print appianErrors
  mapM_ print clientErrors
  putStrLn $ "Successfully executed: " <> tshow (length successes)
  putStrLn $ "Appian Errors: " <> tshow (length appianErrors)
  putStrLn $ "Client Errors: " <> tshow (length clientErrors)

    -- This needs to be replaced as soon as ClientM is generalized in servant-client
loggingFunc :: FilePath -> IO ()
loggingFunc fp = S.takeWhile isMsg
  >>> S.map unpackMsg
  >>> S.writeFile fp
  >>> runResourceT
    $ S.repeatM (atomically $ readTChan logChan)
  where
    isMsg (Msg _) = True
    isMsg _ = False
    unpackMsg (Msg t) = unpack t
    unpackMsg _ = error "The impossible happened unpacking the log Message!"

commandsInfo :: ParserInfo (IO ())
commandsInfo = info (helper <*> parseCommands)
  (  fullDesc
  <> progDesc "Various scripts written for the EPC system."
  )

parseCommands :: Parser (IO ())
parseCommands = subparser
  (  command "scripts" scriptsInfo
  <> command "form486Intake" form486Info
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
  <*> strOption
  (  long "user-csv-prefix"
  <> short 'i'
  )
  <*> option auto
  (  long "num-runs"
  <> short 'n'
  )
  <*> option parseManyR
  (  long "num-users-list"
  <> short 'u'
  )
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
