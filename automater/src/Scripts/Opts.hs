{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scripts.Opts
  ( scriptsInfo
  ) where

import ClassyPrelude
import Options.Applicative

import Scripts.FCCForm471
import Appian.Client (runAppian, LogMessage(..), logChan)
import Appian.Instances
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (hFlush, putChar, getLine, hGetEcho, hSetEcho)
import Control.Arrow
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource
import Data.Aeson (Value)
import Control.Lens

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

runScript :: BaseUrl -> String -> FilePath -> Int -> IO ()
runScript baseUrl username fp nThreads = do
  mgr <- newManager tlsManagerSettings
  password <- getPassword
  let login = Login (pack username) (pack password)
  (_, res) <- concurrently (loggingFunc fp)
              ( do
                  atomically $ writeTChan logChan $ Msg $ "timestamp,elapsed,label,responseCode"
                  results <- mapConcurrently (const $ tryAny $ runAppian form471Intake (ClientEnv mgr baseUrl) login) $ [1..nThreads]
                  atomically $ writeTChan logChan Done
                  return results
              )
  dispResults res

dispResults :: [Either SomeException (Either ServantError Value)] -> IO ()
dispResults results = do
  let appianErrors = results ^.. traverse . _Left
      clientErrors = results ^.. traverse . _Right . _Left
      successes = results ^.. traverse . _Right . _Right
  mapM_ print appianErrors
  mapM_ print clientErrors
  putStrLn $ "Successfully created 471s: " <> tshow (length successes)
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

scriptsInfo :: ParserInfo (IO ())
scriptsInfo = info (helper <*> scriptsParser)
  (  fullDesc
  <> progDesc "Various scripts written for the EPC system."
  )

scriptsParser :: Parser (IO ())
scriptsParser = runScript
  <$> urlParser
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
  (  long "host"
  <> short 'h'
  <> help "The hostname of the server to use."
  )
  <*> option auto
  (  long "port"
  <> short 'p'
  <> help "The port to use for connecting to the remote host."
  )
  <*> pure ""

-- loginParser :: Parser Login
-- loginParser = Login
--   <$> ( pack <$>
--           strOption
--             (  long "username"
--             <> short 'u'
--             )
--       )
--   <*> ( pack <$>
--           strOption
--             (  long "password"
--             <> short 'p'
--             )
--       )
