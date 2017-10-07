{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.Util where

import ClassyPrelude
import qualified Streaming.Prelude as S
import Stats.CsvStream
import Appian.Client
import Appian.Instances
import Servant.Client
import Control.Arrow
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Lens

checkUsers :: (MonadBaseControl IO m, MonadThrow m, MonadIO m) => ClientEnv -> FilePath -> FilePath -> m String
checkUsers env fp outFile = check fp
  where
    f login = do
      res <- tryAny $ runAppian recordsTab env login
      case res of
        Right (Right _) -> return $ Just login
        _ -> return Nothing
    check = csvStreamByName
      >>> S.mapM (liftBase . f)
      >>> S.filter isJust
      >>> S.map (unpack . toRow)
      >>> S.writeFile outFile
      >>> runResourceT
      >>> runNoLoggingT
    toRow (Just login) = login ^. username <> "," <> login ^. password
