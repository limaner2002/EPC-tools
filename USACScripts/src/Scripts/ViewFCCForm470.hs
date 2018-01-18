{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.ViewFCCForm470 where

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

clickForm470Number :: RapidFire m => GridWidgetCell -> AppianT m Value
clickForm470Number gridCell = do
    let mRref = gridCell ^? _GWLink . traverse . recordLinkRef
    case mRref of
        Nothing -> fail "Could not find the record link!"
        Just rref -> viewRecordDashboard rref (Dashboard "summary")

-- viewForm470 :: Login -> Appian Value
viewForm470 :: Login -> Appian Value
viewForm470 _ = do
    viewRecordByName "FCC Forms 470"
    sendRecordUpdates "Select 'Certified Status'" (dropdownUpdateF1 "Status" "Certified")
    mGridWidget <- usesValue (^? getGridWidget) :: Appian (Maybe (GridWidget GridWidgetCell))
    case mGridWidget of
        Nothing -> fail "Could not find the record grid!"
        Just gridWidget -> arbitraryGridRowByColName "FCC Form 470 Number" clickForm470Number gridWidget
    --use appianValue

runViewForm470 :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runViewForm470 = runIt viewForm470
