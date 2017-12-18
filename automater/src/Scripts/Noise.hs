{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scripts.Noise where

import ClassyPrelude
import Appian.Client -- hiding (sendUpdates)
import Appian
import Appian.Instances
import Scripts.Common
import Data.Aeson
-- import Scripts.Opts (runIt)

noise :: Login -> Appian ()
noise _ = do
    _ <- reportsTab
    _ <- recordsTab
    _ <- actionsTab
    _ <- tasksTab Nothing
    return ()

-- runNoise :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> Int -> IO [Either SomeException ()]
-- runNoise = runIt noise
