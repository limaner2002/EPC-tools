{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Opts
  ( schedulerInfo
  ) where

import ClassyPrelude
import Options.Applicative
import Network.Wai.Handler.Warp
import Control.Arrow

import Scheduler.Server
import Scheduler.Types (emptyQueue)

runServer :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> Port -> IO ()
runServer mkJob jobAct port = do
  v <- newTVarIO emptyQueue
  run port $ application mkJob jobAct v

schedulerInfo :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> ParserInfo (IO ())
schedulerInfo mkJob jobAct = info (helper <*> schedulerParser mkJob jobAct)
  (  fullDesc
  <> header "Test scheduler UI."
  <> progDesc "Runs a simple server which allows for scheduling of tests."
  )

schedulerParser :: (Text -> ServantHandler a) -> Kleisli ServantHandler a () -> Parser (IO ())
schedulerParser mkJob jobAct = runServer <$>
  pure mkJob
  <*>
  pure jobAct
  <*>
  option auto
  (  long "port"
  <> short 'p'
  <> metavar "PORT"
  <> help "The port to run the server on."
  )
