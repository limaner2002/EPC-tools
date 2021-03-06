{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.ViewFCCForm471 where

import ClassyPrelude
import Appian.Client
import Appian
import Appian.Instances
import Appian.Types
import Appian.Lens
import Scripts.Execute
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Stats.CsvStream
import Control.Lens
import Control.Lens.Action.Reified
import Scripts.Common

clickForm471Number :: RapidFire m => GridWidgetCell -> AppianT m Value
clickForm471Number gridCell = do
    let mRref = gridCell ^? _GWLink . traverse . recordLinkRef
    case mRref of
        Nothing -> fail "Could not find the record link!"
        Just rref -> viewRecordDashboard rref (Dashboard "summary")

viewForm471 :: Login -> Appian Value
viewForm471 _ = do
    viewRecordByName "FCC Forms 471"
    sendRecordUpdates "Select 'Certified Status'" (dropdownUpdateF1 "Status" "Certified")
    sendRecordUpdates "Select 'Funding Year 2018'" (dropdownUpdateF1 "Funding Year" "2018")
    mGridWidget <- usesValue (^? getGridWidget) :: Appian (Maybe (GridWidget GridWidgetCell))
    case mGridWidget of
        Nothing -> fail "Could not find the record grid!"
        Just gridWidget -> arbitraryGridRowByColName "FCC Form 471 Number" clickForm471Number gridWidget

runViewForm471 :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runViewForm471 = runIt viewForm471
