{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scripts.Opts
  ( scriptsInfo
  ) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Types

import Scripts.FCCForm471
import Scripts.CreateCSCase
import Appian.Client (runAppian, LogMessage(..), logChan)
import Appian.Instances
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
  let login = Login (pack username) (pack password)
  (_, res) <- concurrently (loggingFunc fp)
              ( do
                  atomically $ writeTChan logChan $ Msg $ "timeStamp,elapsed,label,responseCode"
                  results <- mapConcurrently (const $ tryAny $ runAppian script (ClientEnv mgr baseUrl) login) $ [1..nThreads]
                  atomically $ writeTChan logChan Done
                  return results
              )
  dispResults res

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
  -- case res of
  --   Left exc -> print exc
  --   Right res' -> case res' of
  --     Left exc -> print exc
  --     Right _ -> putStrLn "FCC Form 471 created successfully!"

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
  <> short 't'
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

scriptParser :: ReadM Script
scriptParser = do
  name <- readerAsk
  case name of
    "cscase" -> return $ CSScript createCSCase
    "form471" -> return $ Form471 form471Intake
    _ -> fail $ show name <> " is not a valid script name!"
