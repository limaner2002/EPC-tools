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

createCsCase :: Login -> Appian Value
createCsCase _ = do
    executeActionByName "Create a Customer Service Case"
    sendUpdates1 "Enter the Title" (textUpdateF "Title" "PerfTest11")
    sendUpdates1 "Enter the Description" (paragraphUpdateF "Description" "This is my description!")
    sendUpdates1 "Select Topic" (dropdownUpdateF "Topic" 5)
    sendUpdates1 "Select Subtopic" (dropdownUpdateF "Subtopic" 2)
    sendUpdates1 "Select Priority" (dropdownUpdateF "Priority" 3)
    sendUpdates1 "Select Inquiry Type" (dropdownUpdateF "Inquiry Type" 3)
    sendUpdates1 "Enter the Case Contact Information First Name" (textUpdateF "First Name" "Car121")
    sendUpdates1 "Enter the Case Contact Information Last Name" (textUpdateF "Last Name" "SLC1")
    sendUpdates1 "Enter the Case Contact Information Email" (textUpdateF "Email" "car12@mailinator.com")
    sendUpdates1 "Enter the Case Contact Information Phone" (textUpdateF "Phone" "123-456-7890")
    sendUpdates1 "Click on 'Submit' button" (buttonUpdateF "Submit")
    use appianValue

-- This is the function that I need so that I can include it in the test.
runCreateCsCase :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runCreateCsCase = runIt createCsCase
