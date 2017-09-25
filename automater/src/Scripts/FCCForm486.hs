{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.FCCForm486 where

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
import Data.Attoparsec.Text

form486Intake :: AppianUsername -> Appian (Maybe Text)
form486Intake appianUN = do
  let un = Identifiers [appianUN]
  v <- myLandingPageAction "FCC Form 486"

  v' <- case v ^? hasKeyValue "label" "Existing Organizations" of
    Nothing -> return v
    Just _ -> sendUpdates "Select Organization" ( gridFieldArbitrarySelect
                                                <|> MonadicFold (to (buttonUpdate "Create FCC Form 486"))
                                                ) v

  sendUpdates' "Nickname" ( MonadicFold (textFieldArbitrary "Nickname" 255)
                                        <|> MonadicFold (to (dropdownUpdate "Funding Year" 2))
                                        <|> MonadicFold (to (pickerUpdate "Main Contact Person" un))
                                        <|> MonadicFold (to (buttonUpdate "Continue"))
                                      ) v'
    >>= handleValidations
    >>= sendUpdates "Add All FRNs & Continue" (addAllFRNsButtonUpdate
                                              <|> MonadicFold (to (buttonUpdate "Continue"))
                                              )
    >>= sendUpdates "Service Information" (MonadicFold (to (buttonUpdate "Continue")))
    >>= sendUpdates "Check Early Filing, CIPA Waiver & Continue" (MonadicFold (to (checkboxGroupUpdate "CHECK THE BOX BELOW IF THE FRNS ON THIS FCC FORM 486 ARE FOR SERVICES STARTING ON OR BEFORE JULY 31 OF THE FUNDING YEAR." [1]))
                                                                 <|> MonadicFold (to (buttonUpdate "Continue"))
                                                                 )
   >>= sendUpdates "Certifications & Preview" (MonadicFold (to (checkboxGroupUpdate "Certifications" [1,2]))
                                              <|> MonadicFold (to (radioButtonFieldUpdate "" 1))
                                              <|> MonadicFold (to (buttonUpdate "Preview"))
                                              )
   >>= handleWaiver
   >>= sendUpdates "Click Certify" (MonadicFold (to (buttonUpdate "Certify")))
   >>= \res -> return (res ^? deep (filtered $ has $ key "label" . _String . prefixed "You have successfully") . key "label" . _String . to (parseOnly getNumber) . traverse)

checkboxGroupUpdate :: Text -> [Int] -> Value -> Either Text Update
checkboxGroupUpdate label selection v = toUpdate <$> (_Right . cbgValue .~ Just selection $ cbg)
  where
    cbg = maybeToEither ("Could not locate checkbox group " <> tshow label) $ v ^? getCheckboxGroup label

radioButtonFieldUpdate :: Text -> Int -> Value -> Either Text Update
radioButtonFieldUpdate label selection v = toUpdate <$> (_Right . rdgValue .~ Just (AppianInt selection) $ rdg)
  where
    rdg = maybeToEither ("Could not locate RadioButtonField " <> tshow label) $ v ^? getRadioButtonField label

getNumber :: Parser Text
getNumber = takeTill (== '#') *> Data.Attoparsec.Text.take 1 *> takeTill (== ' ')

handleWaiver :: Value -> Appian Value
handleWaiver v = do
  case v ^? getRadioButtonField "Waiver Clarification" of
    Nothing -> return v
    Just _ -> sendUpdates "Waiver Clarification" (MonadicFold (to $ radioButtonFieldUpdate "Waiver Clarification" 1)
                                                 <|> MonadicFold (to (buttonUpdate "Preview"))
                                                 ) v
