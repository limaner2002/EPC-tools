{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.ComadIntake where

import Appian.Client
import Scripts.Common
import Scripts.ReviewCommon
import Scripts.ComadReview
import Control.Monad.Logger
import ClassyPrelude
import Data.Aeson
import Control.Lens
import Control.Lens.Action.Reified
import Appian
import Appian.Instances
import Appian.Types
import Appian.Lens
import Scripts.Execute
import Stats.CsvStream
import Control.Monad.Except
import Appian.Internal.Arbitrary

comadIntake :: Login -> Appian Value
comadIntake _ = do
    executeActionByName "Initiate COMAD"
    sendUpdates1 "Select Funding Year" (dropdownUpdateF1 "Funding Year" "2017")
    sendUpdates1 "Click 'No' Button" (buttonUpdateF "No")
    mGF <- usesValue (^? getGridFieldCell . traverse)
    case mGF of
        Nothing -> fail "Could not find grid field!"
        Just gf -> sendUpdates1 "Select FRN in grid" (selectGridfieldUpdateF (IntIdent $ AppianInt 24) gf)
    sendUpdates1Handle notPrimaryOrgSelected "Click 'Add FRNs' button" (buttonUpdateWithF isAddAllButton "Could not find Add FRNs button")
    mComponents <- usesValue (^? getGridWidget . asGWC . gwVal . traverse . _2 . runFold (
                                 (,,)
                                 <$> Fold (at "Primary FRN" . traverse . _GWCheckbox)
                                 <*> Fold (at "Recovery Type" . traverse . _GWDropdown)
                                 <*> Fold (at "Adjustment Reason" . traverse . _GWDropdown)
                                 )
                             )
    case mComponents of
      Nothing -> fail "Could not find components"
      Just tpl -> rowUpdate tpl

    sendUpdates1 "Click 'Continue' Button" (buttonUpdateF "Continue")
    sendUpdates1 "Enter 'Nickname'" (textFieldArbitraryF "Nickname" 255)
    sendUpdates1 "Enter 'Narrative'" (paragraphArbitraryUpdate "Narrative" 4000)
    sendUpdates1 "Select 'Origin'" (dropdownArbitraryUpdateF "Origin")

    sendUpdates1 "Click 'Submit' Button" (buttonUpdateF "Submit")
    use appianValue
    
notPrimaryOrgSelected :: ScriptError -> Bool
notPrimaryOrgSelected (ValidationsError (["Please select an FRN to determine the primary affected organizations"], _, _)) = True
notPrimaryOrgSelected _ = False

isAddAllButton :: Text -> Bool
isAddAllButton label = isPrefixOf "Add" label && isSuffixOf "FRNs" label

runComadIntake :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runComadIntake = runIt comadIntake

rowUpdate :: (CheckboxField, DropdownField, DropdownField) -> Appian ()
rowUpdate (cbox, recoveryDf, adjustmentDf) = do
  let cbox' = cbfValue .~ (Just [1]) $ cbox

  recoveryDf' <- dropdownArbitrarySelect recoveryDf
  adjustmentDf' <- dropdownArbitrarySelect adjustmentDf
  sendUpdates1 "Select 'Primary FRN' Checkbox" (componentUpdateWithF "Could not find primary FRN Checkbox" $ to $ const $ cbox')
  sendUpdates1 "Select 'Recovery Type' Checkbox" (componentUpdateWithF "Could not find primary Recovery Type Checkbox" $ to $ const $ recoveryDf')
  sendUpdates1 "Select 'Adjustment Type' Checkbox" (componentUpdateWithF "Could not find primary Adjustment Type Checkbox" $ to $ const $ adjustmentDf')
  
asGWC = to (id :: GridWidget GridWidgetCell  -> GridWidget GridWidgetCell)
