{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.ComadIntake where

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Appian.Client
import Scripts.Common
import Scripts.ReviewCommon
import Scripts.ComadReview
import NewFunctions
import Control.Monad.Logger
import ClassyPrelude
import Data.Aeson
import Control.Lens
import Control.Lens.Action.Reified
import Appian
import Appian.Instances
import Appian.Types
import Appian.Lens
import Scripts.Opts
import Stats.CsvStream
import Control.Monad.Except

comadIntake :: Login -> Appian Value
comadIntake _ = do
    executeActionByName "Initiate COMAD"
    sendUpdates1 "Select Funding Year" (dropdownUpdateF1 "Funding Year" "2016")
    mGF <- usesValue (^? getGridFieldCell . traverse)
    case mGF of
        Nothing -> fail "Could not find grid field!"
        Just gf -> sendUpdates1 "Select FRN in grid" (selectGridfieldUpdateF (IntIdent $ AppianInt 1) gf)
    sendUpdates1Handle notPrimaryOrgSelected "Click 'Add FRNs' button" (buttonUpdateWithF isAddAllButton "Could not find Add FRNs button")
    use appianValue
    
notPrimaryOrgSelected :: ScriptError -> Bool
notPrimaryOrgSelected (ValidationsError (["Please select an FRN to determine the primary affected organizations"], _, _)) = True
notPrimaryOrgSelected _ = False

isAddAllButton :: Text -> Bool
isAddAllButton label = isPrefixOf "Add" label && isSuffixOf "FRNs" label

runComadIntake :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runComadIntake = runIt comadIntake
