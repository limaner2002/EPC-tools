{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

import ClassyPrelude
import Appian.Client
import Appian
import Appian.Instances
import Appian.Types
import Appian.Lens
import Scripts.Opts
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Stats.CsvStream
import Control.Lens
import Control.Lens.Action.Reified
import Scripts.Common
import qualified Data.Csv as Csv
import NewFunctions
import Control.Monad.Except (throwError)

data ServiceSubConf = ServiceSubConf 
    { _appNumber :: Text
    , _applicant :: Login
    } deriving Show
    
makeLenses ''ServiceSubConf

instance HasLogin ServiceSubConf where
    getLogin conf = conf ^. applicant

instance Csv.FromNamedRecord ServiceSubConf where
    parseNamedRecord bs = ServiceSubConf
        <$> bs Csv..: "appNumber"
        <*> Csv.parseNamedRecord bs

serviceSubstitution :: ServiceSubConf -> Appian Value
serviceSubstitution conf = do
    let un = Identifiers $ conf ^.. applicant . username . to AppianUsername 
    --clickServiceSubstitution
    myLandingPageAction1 "Service Substitution"
    sendUpdates1 "Nickname" (textUpdateF "Nickname" "myNickname")

    sendUpdates1 "Select 'Funding Year'" (dropdownUpdateF1 "Funding Year" "2017")
    sendUpdates1 "Select 'Main Contact Person'" (pickerUpdateF "Main Contact Person" un)
    sendUpdates1Handle ignoreAssociateValidation "click Continue" (buttonUpdateF "Continue")

    sendUpdates1 "Select FRN in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    sendUpdates1 "Click 'Add FRNs' button" (buttonUpdateWithF isAddButton "Could not find Add FRNs button")

    sendUpdates1 "Select FRN Line Item in grid" (gridFieldUpdateWithF (dropping 2 getGridFieldCell) 0)
    sendUpdates1 "Click 'Add Line Items' button" (buttonUpdateWithF isAddLineItemButton "Could not find Add Line Items button")

    sendUpdates1 "Click 'Continue' button" (buttonUpdateF "Continue")
    use appianValue
    
isAddButton :: Text -> Bool
isAddButton label = isPrefixOf "Add (" label && isSuffixOf ") FRNs" label

isAddLineItemButton :: Text -> Bool
isAddLineItemButton label = isPrefixOf "Add (" label && isSuffixOf ") Line Items" label

runServiceSubstitution :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runServiceSubstitution = runIt serviceSubstitution
