{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.SPINChange where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Control.Lens.Action.Reified
import Scripts.Test
import Scripts.Common

spinChangeIntake :: Appian Value
spinChangeIntake = do
  let un = Identifiers [AppianUsername "kyle.davie@fwisd.org"]

  myLandingPageAction "SPIN Change"
    >>= sendUpdates' "Nickname" (MonadicFold (textFieldArbitrary "Nickname" 255)
                                <|> MonadicFold (to (dropdownUpdate "Funding Year" 2))
                                <|> MonadicFold (to (pickerUpdate "Main Contact Person" un))
                                <|> MonadicFold (to (buttonUpdate "Continue"))
                               )
    >>= handleValidations
    >>= (\v -> sendUpdates "Search for FRNs with SPIN" (MonadicFold (to (textUpdate "SPIN" "143008158"))
                                   <|> MonadicFold (to (buttonUpdate "Search"))
                                   ) v
               >> return v
        )
    >>= sendUpdates "Add All FRNs Button & Continue" (addAllFRNsButtonUpdate
                                                      <|> MonadicFold (to (buttonUpdate "Continue"))
                                                     )
    >>= sendUpdates "Select Reason" (MonadicFold (to (dropdownUpdate "Please select the reason why you would like to change the service provider on the FRN(s)" 2))
                                    <|> MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" "143000824"))
                                    )

