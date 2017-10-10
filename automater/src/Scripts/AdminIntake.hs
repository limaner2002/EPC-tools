{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.AdminIntake where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action.Reified
import Scripts.Common
import Scripts.Test
import Scripts.ReviewCommon

adminIntake :: AppianUsername -> Appian (Maybe Text)
adminIntake un = do
  let user = Identifiers [un]

  v <- myLandingPageAction "Appeal"

  v' <- case v ^? hasKeyValue "label" "Existing Organizations" of
    Nothing -> return v
    Just _ -> sendUpdates "Select Organization" ( gridFieldArbitrarySelect
                                                <|> MonadicFold (to (buttonUpdate "Create Appeal"))
                                                ) v
  sendUpdates "Appeal Details & Contact"
          (MonadicFold (textFieldArbitrary "Nickname" 255)
            <|> MonadicFold (to $ dropdownUpdate "Funding Year" 3)
            <|> dropdownArbitraryUpdateF "What type of decision would you like to appeal?"
            <|> dropdownArbitraryUpdateF "Appeal Category"
--            <|> dropdownUpdateF' "Appeal Type" RevAdminCorrection
            <|> dropdownArbitraryUpdateF "Appeal Type"
            <|> MonadicFold (to $ pickerUpdate "Main Contact Person" user)
            <|> MonadicFold (to $ buttonUpdate "Continue")
          ) v'
    >>= sendUpdates' "Choose Search Method" (MonadicFold (to $ buttonUpdate "Search by FRN")
                                           <|> MonadicFold (to $ buttonUpdate "Continue")
                                           )
    >>= handleValidations
    >>= sendUpdates "Add All FRNs Button" (addAllFRNsButtonUpdate
                                           <|> MonadicFold (to (buttonUpdate "Continue"))
                                          )
    >>= sendUpdates "Narrative & Submit" (paragraphArbitraryUpdate "Narrative" 2000
                                         <|> MonadicFold (to (buttonUpdate "Submit"))
                                         )
    >>= \res -> return (res ^? deep (filtered $ has $ key "#v" . _String . prefixed "You have successfully created Appeal") . key "#v" . _String . to parseNumber . traverse)
