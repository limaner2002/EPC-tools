{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scripts.FCCForm500Intake
  ( runForm500Intake
  ) where

import ClassyPrelude
import Appian.Client
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Internal.Arbitrary
import Appian
import Data.Aeson
import Stats.CsvStream
import Scripts.Execute
import Control.Lens
import Scripts.Common
import Control.Monad.Time
import Data.Time
import Control.Lens
import qualified Data.Csv as Csv

data Form500Conf = Form500Conf
  { _applicant :: Login
  , _appMultiOrgMethod :: SelectOrgMethod
  } deriving Show

makeLenses ''Form500Conf

instance Csv.FromNamedRecord Form500Conf where
  parseNamedRecord bs = Form500Conf
    <$> Csv.parseNamedRecord bs
    <*> bs Csv..: "Select Org Method"

instance HasLogin Form500Conf where
  getLogin conf = conf ^. applicant

form500Intake :: Form500Conf -> Appian Value
form500Intake conf = do
    let un = Identifiers $ conf ^.. applicant . username . to AppianUsername
    --executeActionByName "FCC Form 500"
    myLandingPageAction1 "FCC Form 500"

    handleMultipleEntities checkMultiOrgs "Create FCC Form 500" "Billed Entity Name" (conf ^. appMultiOrgMethod)
    --isButton
    sendUpdates1 "select First as NO" (componentUpdateWithF "Could not find first YES/NO button" $ taking 1 $ getButton "No")
    sendUpdates1 "Select Funding Year" (dropdownUpdateF1 "Funding Year" "2017")
    sendUpdates1 "Type Nickname" (textFieldArbitraryF "Nickname" 255)
    sendUpdates1 "Type Main Contact Person" (pickerUpdateF "Main Contact Person" un)
    -- sendUpdates1 "Select Main Contact Person" (dropdownUpdateF1 "Funding Year" "2016")
    
    sendUpdates1 "Select Application Number in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    
    sendUpdates1 "select Second as YES" (componentUpdateWithF "Could not find second YES/NO button" $ taking 1 $ dropping 1 $ getButton "Yes")
    sendUpdates1 "select Remaining as No" (componentUpdateWithF "Could not find remaining YES/NO buttons" $ dropping 1 $ getButton "No")
    
    
    sendUpdates1Handle notPrimaryOrgSelected "click Continue" (buttonUpdateF "Continue")
    --sendUpdates1 "click Continue" (buttonUpdateF "Continue")
    
    
    sendUpdates1 "Select FRN in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    sendUpdates1 "Click 'Add FRNs' button" (buttonUpdateWithF isAddButton "Could not find Add FRNs button")
    sendUpdates1 "Enter the Description" (paragraphUpdateF "Please provide an explanation or further detail about why you are requesting a change to your service start date." "This is my description!")
    
    newDate <- AppianDate . Just . addDays 30 . utctDay <$> currentTime
    sendUpdates1 "Enter New Service Start Date" (datePickerUpdateF "New SSD 1" newDate)

    sendUpdates1 "Click 'Continue' button" (buttonUpdateF "Continue")
    
    sendUpdates1 "Check box 1" (checkboxGroupUpdateF "Certifications" [1,2,3])
    sendUpdates1 "Click Certify" (buttonUpdateF "Certify")
    
    use appianValue

notPrimaryOrgSelected :: ScriptError -> Bool
notPrimaryOrgSelected (ValidationsError (["You must associate at least one Funding Request"], _, _)) = True
notPrimaryOrgSelected _ = False

isAddButton :: Text -> Bool
isAddButton label = isPrefixOf "Add (" label && isSuffixOf ") FRNs" label

runForm500Intake :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runForm500Intake = runIt form500Intake
    
checkMultiOrgs :: Value -> MultiOrgStatus
checkMultiOrgs v = case has (hasLabel "Existing Organizations" . asValue) v of
  True -> trace "Single organization" IsMultiple
  False -> trace "Multiple organizations" IsSingle
