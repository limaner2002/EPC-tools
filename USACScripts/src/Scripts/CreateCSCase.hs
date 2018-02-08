{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.CreateCSCase where

import ClassyPrelude
import Appian.Client
import Appian
import Appian.Instances
import Scripts.Execute
import Appian.Types
import Appian.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Stats.CsvStream
import Control.Lens
import Appian.Internal.Arbitrary

createCsCase :: Login -> Appian Value
createCsCase _ = do
    executeActionByName "Create a Customer Service Case"
    sendUpdates1 "Enter the Title" (textFieldArbitraryF "Title" 100)
    sendUpdates1 "Enter the Description" (paragraphArbitraryUpdate "Description" 2000)
    sendUpdates1 "Select Topic" (dropdownArbitraryUpdateF "Topic")
    sendUpdates1 "Select Subtopic" (dropdownArbitraryUpdateF "Subtopic")
    sendUpdates1 "Select Priority" (dropdownArbitraryUpdateF "Priority")
    sendUpdates1 "Select Inquiry Type" (dropdownArbitraryUpdateF "Inquiry Type")
    sendUpdates1 "Enter the Case Contact Information First Name" (textFieldArbitraryF "First Name" 35)
    sendUpdates1 "Enter the Case Contact Information Last Name" (textFieldArbitraryF "Last Name" 35)
    sendUpdates1 "Enter the Case Contact Information Email" (textUpdateF "Email" "car12@mailinator.com")
    sendUpdates1 "Enter the Case Contact Information Phone" (textUpdateF "Phone" "123-456-7890")
    sendUpdates1 "Click on 'Submit' button" (buttonUpdateF "Submit")
    use appianValue

-- This is the function that I need so that I can include it in the test.
runCreateCsCase :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runCreateCsCase = runIt createCsCase
