{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats.CsvStream where

import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as S
import Data.Csv.Streaming
import Data.Csv (FromRecord, FromNamedRecord, Header)
import Control.Arrow
import ClassyPrelude
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Logger

csvStream :: (MonadResource m, FromRecord a) => HasHeader -> FilePath -> S.Stream (S.Of a) m ()
csvStream hasHeader path = do
  ct <- BSS.readFile >>> BSS.toLazy $ path
  S.unfoldr (unfoldRecords hasHeader) $ decode hasHeader $ S.fst' ct

unfoldRecords :: (MonadIO m, FromRecord a) => HasHeader -> Records a -> m (Either () (a, Records a))
unfoldRecords hasHeader recs = loop recs
  where
    loop (Cons (Left msg) resume) = liftIO (putStrLn $ pack msg) >> loop resume
    loop (Cons (Right a) resume) = return $ Right (a, resume)
    loop (Nil _ rest)
      | onull rest = return $ Left ()
      | otherwise = loop $ decode hasHeader rest

csvStreamByName :: (MonadResource m, FromNamedRecord a, MonadLogger m) => FilePath -> S.Stream (S.Of a) m String
csvStreamByName path = do
  ct <- BSS.readFile >>> BSS.toLazy $ path
  S.unfoldr unfoldRecordsByName $ decodeByName $ S.fst' ct

unfoldRecordsByName :: (MonadLogger m, FromNamedRecord a) => Either String (Header, Records a) -> m (Either String (a, Either String (Header, Records a)))
unfoldRecordsByName eRecs = loop eRecs
  where
    loop (Left msg) = do
      logDebugN $ pack msg
      return $ Left msg
    loop (Right (h, Cons (Left msg) resume)) = do
      logDebugN $ pack msg
      loop $ Right (h, resume)
    loop (Right (h, Cons (Right a) resume)) = return $ Right (a, Right (h, resume))
    loop (Right (h, Nil msg rest))
      | onull rest = do
          logIt msg
          return $ Left "Done"
      | otherwise = do
          logIt msg
          loop $ decodeByName rest
      where
        logIt (Just msg) = logDebugN $ pack msg
        logIt _ = return ()