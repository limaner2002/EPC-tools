{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.Time
import Data.Random (MonadRandom)

form486Intake :: (MonadCatch m, MonadLogger m, MonadTime m, MonadGen m, RunClient m, MonadBase IO m, MonadRandom m) => Login -> AppianT m (Maybe Text)
form486Intake login = do
  let un = Identifiers [login ^. username . to AppianUsername]
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
    >>= sendUpdates "Check Early Filing, CIPA Waiver & Continue" (MonadicFold (checkboxGroupUpdate "CHECK THE BOX BELOW IF THE FRNS ON THIS FCC FORM 486 ARE FOR SERVICES STARTING ON OR BEFORE JULY 31 OF THE FUNDING YEAR." [1])
                                                                 <|> MonadicFold (to (buttonUpdate "Continue"))
                                                                 )
   >>= sendUpdates "Certifications & Preview" (MonadicFold (checkboxGroupUpdate "Certifications" [1,2])
                                              <|> MonadicFold (to (radioButtonFieldUpdate "" 1))
                                              <|> MonadicFold (to (buttonUpdate "Preview"))
                                              )
   >>= handleWaiver
   >>= sendUpdates "Click Certify" (MonadicFold (to (buttonUpdate "Certify")))
   >>= \res -> return (res ^? deep (filtered $ has $ key "label" . _String . prefixed "You have successfully") . key "label" . _String . to parseNumber . traverse)

radioButtonFieldUpdate :: Text -> Int -> Value -> Either Text Update
radioButtonFieldUpdate label selection v = toUpdate <$> (_Right . rdgValue .~ Just (AppianInteger selection) $ rdg)
  where
    rdg = maybeToEither ("Could not locate RadioButtonField " <> tshow label) $ v ^? getRadioButtonField label

handleWaiver :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
handleWaiver v = do
  case v ^? getRadioButtonField "Waiver Clarification" of
    Nothing -> return v
    Just _ -> sendUpdates "Waiver Clarification" (MonadicFold (to $ radioButtonFieldUpdate "Waiver Clarification" 1)
                                                 <|> MonadicFold (to (buttonUpdate "Preview"))
                                                 ) v
