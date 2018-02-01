{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Scripts.ServiceSubstitutionIntake
  ( runServiceSubstitution
  ) where

import ClassyPrelude
import Appian.Client
import Appian
import Appian.Instances
import Appian.Types
import Appian.Lens
import Appian.Internal.Arbitrary
import Scripts.Execute
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Stats.CsvStream
import Control.Lens
import Control.Lens.Action.Reified
import Scripts.Common
import qualified Data.Csv as Csv
import Control.Monad.Except (throwError)

data ServiceSubConf = ServiceSubConf 
    { _appNumber :: Text
    , _applicant :: Login
    , _appMultiOrgMethod :: SelectOrgMethod
    } deriving Show
    
makeLenses ''ServiceSubConf

instance HasLogin ServiceSubConf where
    getLogin conf = conf ^. applicant

instance Csv.FromNamedRecord ServiceSubConf where
    parseNamedRecord bs = ServiceSubConf
        <$> bs Csv..: "appNumber"
        <*> Csv.parseNamedRecord bs
        <*> bs Csv..: "Select Org Method"

serviceSubstitution :: ServiceSubConf -> Appian Value
serviceSubstitution conf = do
    let un = Identifiers $ conf ^.. applicant . username . to AppianUsername 
    --clickServiceSubstitution
    myLandingPageAction1 "Service Substitution" 

    handleMultipleEntities checkMultiOrgs "Create Service Substitution" "Billed Entity Name" (conf ^. appMultiOrgMethod)

    sendUpdates1 "Nickname" (textFieldArbitraryF "Nickname" 255)

    sendUpdates1 "Select 'Funding Year'" (dropdownUpdateF1 "Funding Year" "2017")
    sendUpdates1 "Select 'Main Contact Person'" (pickerUpdateF "Main Contact Person" un)
    sendUpdates1Handle ignoreAssociateValidation "click Continue" (buttonUpdateF "Continue")

    sendUpdates1 "Select FRN in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    sendUpdates1 "Click 'Add FRNs' button" (buttonUpdateWithF isAddButton "Could not find Add FRNs button")

    sendUpdates1 "Select FRN Line Item in grid" (gridFieldUpdateWithF (dropping 2 getGridFieldCell) 0)
    sendUpdates1 "Click 'Add Line Items' button" (buttonUpdateWithF isAddLineItemButton "Could not find Add Line Items button")

    sendUpdates1 "Click 'Continue' button" (buttonUpdateF "Continue")

    clickEachLineItemLink
    sendUpdates1 "Click 'Continue' button to Additional Questions" (buttonUpdateF "Continue")

    sendUpdates1 "Select all as 'Yes'" (componentUpdateWithF "Could not find 1 or more of the 'Yes' buttons on the 'Additional Questions' page." $ getButton "Yes")
    sendUpdates1 "Select 'Preview' button" (buttonUpdateF "Preview")
    sendUpdates1 "Select 'Submit' button" (buttonUpdateF "Submit")
    use appianValue

isAddButton :: Text -> Bool
isAddButton label = isPrefixOf "Add (" label && isSuffixOf ") FRNs" label

isAddLineItemButton :: Text -> Bool
isAddLineItemButton label = isPrefixOf "Add (" label && isSuffixOf ") Line Items" label

runServiceSubstitution :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runServiceSubstitution = runIt serviceSubstitution

clickEachLineItemLink :: RapidFire m => AppianT m ()
clickEachLineItemLink = void $ forGridRows1_ sendUpdates (^. gfColumns . at "FRN Line Item Number" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridField . traverse) clickLineItemLink

clickLineItemLink :: RapidFire m => DynamicLink -> GridField GridFieldCell -> AppianT m ()
clickLineItemLink dyl _ = do
  sendUpdates1 "Click FRN Line Item Number" (MonadicFold $ to $ const $ Right $ toUpdate dyl)
  sendUpdates1 "Click 'Save Line Item Details'" (buttonUpdateF "Save Line Item Details")

checkMultiOrgs :: Value -> MultiOrgStatus
checkMultiOrgs v = case has (hasLabel "Existing Organizations" . asValue) v of
  True -> trace "Single organization" IsMultiple
  False -> trace "Multiple organizations" IsSingle
