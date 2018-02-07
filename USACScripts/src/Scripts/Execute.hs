{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scripts.Execute where

import ClassyPrelude
import Stats.CsvStream
import Appian.Client
import Appian.Instances
import Appian
import Scripts.Common
import qualified Streaming.Prelude as S
import qualified Data.Csv as Csv
import Network.HTTP.Client ( newManager, managerModifyResponse, ResponseTimeout, ManagerSettings
                           , managerResponseTimeout, responseTimeoutMicro
                           )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.Trans.Resource
import Servant.Client
import Control.Monad.Logger
import Scripts.ProducerConsumer
import Control.Arrow
import qualified Control.Concurrent.Async.Pool as Pool

runIt :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError b))]
runIt f bounds (HostUrl hostUrl) logMode csvInput (RampupTime delay) (NThreads n) = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState bounds

  runResourceT $ runStdoutLoggingT $ runParallel $ Parallel (nThreads n) (S.zip (S.each [0..]) $ void (csvStreamByName csvInput)) (\(i, a) -> do
                                                                                                               let d = (i * (delay `div` n))
                                                                                                               threadDelay $ trace (show d) d
                                                                                                               res <- liftIO $ runAppianT logMode (f a) appianState env (getLogin a)
                                                                                                               logResult res
                                                                                                               return res
                                                                                                           )
newtype RampupTime = RampupTime Int
  deriving (Show, Eq, Num)

mkRampup :: Int -> RampupTime
mkRampup n = RampupTime $ n * 1000000

setTimeout :: ResponseTimeout -> ManagerSettings -> ManagerSettings
setTimeout timeout settings = settings { managerResponseTimeout = timeout }

logResult :: (MonadLogger m, MonadThreadId m) => Either ServantError (Either ScriptError a) -> m ()
logResult (Left err) = do
  tid <- threadId
  logErrorN $ tshow tid <> ": " <> tshow err
logResult (Right (Left err)) = do
  tid <- threadId
  logErrorN $ tshow tid <> ": " <> tshow err
logResult (Right (Right _)) = return ()

exhaustiveProducer :: (Csv.FromNamedRecord a, MonadResource m, MonadLogger m) => TBQueue a -> CsvPath -> m String
exhaustiveProducer q = csvStreamByName >>> S.mapM_ (atomically . writeTBQueue q)

                       -- Must take a look at how I can return the results from this.
runScriptExhaustive :: (Csv.FromNamedRecord a, Show a, HasLogin a) => (a -> Appian b) -> Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> NumRecords -> IO ()
runScriptExhaustive f bounds (HostUrl hostUrl) logMode csvInput nThreads (NumRecords numRecords) = do
  mgr <- newManager $ setTimeout (responseTimeoutMicro 90000000000) $ tlsManagerSettings { managerModifyResponse = cookieModifier }
  let env = ClientEnv mgr (BaseUrl Https hostUrl 443 mempty)
      appianState = newAppianState bounds
  confs <- csvStreamByName >>> S.take numRecords >>> S.toList >>> runResourceT >>> runStdoutLoggingT $ csvInput
  execTaskGroup_ nThreads (\a -> runStdoutLoggingT $ do
                             res <- liftIO $ runAppianT logMode (f a) appianState env (getLogin a)
                             logResult res
                         ) $ S.fst' confs

execTaskGroup :: Traversable t => NThreads -> (a -> IO b) -> t a -> IO (t b)
execTaskGroup (NThreads n) f args = Pool.withTaskGroup n $ \group -> Pool.mapConcurrently group f args

execTaskGroup_ :: Traversable t => NThreads -> (a -> IO b) -> t a -> IO ()
execTaskGroup_ n f args = execTaskGroup n f args >> return ()
