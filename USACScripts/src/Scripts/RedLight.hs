{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.RedLight where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Control.Lens
import Control.Lens.Action.Reified
import qualified Data.Foldable as F
import Scripts.Common
import Scripts.Test

redLightReport :: Appian Value
redLightReport = do
  rid <- reportsTab
    >>= getReportId "Red Light Report"

  editReport (PathPiece rid)
    >>= sendReportUpdates rid "Apply Filters" (MonadicFold (taking 1 $ getButton "Apply Filters" . to toUpdate . to Right)
                                              )
