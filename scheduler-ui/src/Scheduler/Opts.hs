{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Opts
  ( schedulerInfo
  ) where

import ClassyPrelude
import Options.Applicative
import Network.Wai.Handler.Warp
import Control.Arrow
import Control.Monad.Trans.Except
import Scheduler.Server
import Scheduler.Types (emptyQueue)
import Servant as S

runServer :: (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> Port -> IO ()
runServer mkJob jobAct port = do
  v <- newTVarIO emptyQueue
  run port $ application (S.Handler . mkJob) (toHandler jobAct) v

schedulerInfo :: (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> ParserInfo (IO ())
schedulerInfo mkJob jobAct = info (helper <*> schedulerParser mkJob jobAct)
  (  fullDesc
  <> header "Test scheduler UI."
  <> progDesc "Runs a simple server which allows for scheduling of tests."
  )

schedulerParser :: (Text -> ExceptT ServantErr IO a) -> Kleisli (ExceptT ServantErr IO) a () -> Parser (IO ())
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

toHandler :: Kleisli (ExceptT ServantErr IO) a b -> Kleisli S.Handler a b
toHandler act = Kleisli (S.Handler . runKleisli act)
