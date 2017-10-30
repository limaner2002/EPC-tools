{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scripts.SPINChangeIntake where

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
import qualified Data.Foldable as F
import Control.Monad.Time

spinChangeIntake :: (RunClient m, MonadTime m, MonadGen m, MonadThrow m, MonadLogger m, MonadCatch m) => AppianT m (Maybe Text)
spinChangeIntake = do
  let un = Identifiers [AppianUsername "app.full.right@testmail.usac.org"]

  v <- myLandingPageAction "SPIN Change"

  v' <- case v ^? hasKeyValue "label" "Existing Organizations" of
    Nothing -> return v
    Just _ -> sendUpdates "Select Organization" ( gridFieldArbitrarySelect
                                                <|> MonadicFold (to (buttonUpdate "Create SPIN Change"))
                                                ) v

  sendUpdates "Nickname & SPIN Change Type" (MonadicFold (textFieldArbitrary "Nickname" 255)
                          <|> MonadicFold (to (dropdownUpdate "SPIN Change Type" 2))
                          ) v'
    >>= sendUpdates' "Funding Year & Main Contact & Continue" (MonadicFold (to (dropdownUpdate "Funding Year" 2))
                                                               <|> MonadicFold (to (pickerUpdate "Main Contact Person" un))
                                                               <|> MonadicFold (to (buttonUpdate "Continue"))
                                                              )
    >>= handleValidations
    >>= selectOldSPIN
    >>= sendUpdates "Add All FRNs Button & Continue" (addAllFRNsButtonUpdate
                                                      <|> MonadicFold (to (buttonUpdate "Continue"))
                                                     )
    >>= \v -> foldGridFieldPages (MonadicFold (getGridFieldCell . traverse)) printFRNs v v
    >>= selectNewSPIN 143999999
    >>= sendUpdates "Click Preview" (MonadicFold (to (dropdownUpdate "Please select the reason why you would like to change the service provider on the FRN(s)" 2))
                                      <|> MonadicFold (to (buttonUpdate "Preview"))
                                    )
    >>= sendUpdates "Click Submit" (MonadicFold (to (buttonUpdate "Submit")))
    >>= \res -> return (res ^? deep (filtered $ has $ key "#v" . _String . suffixed "has been successfully created") . key "#v" . _String . to parseNumber . traverse)

printFRNs :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
printFRNs val gf = do
  v <- foldGridField (printFRN val) "FRN" val gf
  return (v, v)

printFRN :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> Value -> GridFieldCell -> AppianT m Value
printFRN val _ gf = (F.foldlM . F.foldlM) f val $ gf ^.. _TextCellDynLink . _2
  where
    f _ dyl =   sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) val
            >>= sendUpdates "No & Continue" ( MonadicFold (to $ buttonUpdate "No")
                                              <|> MonadicFold (to $ buttonUpdate "Continue")
                                            )

selectNewSPIN :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Int -> Value -> AppianT m Value
selectNewSPIN spin val = do
  v <- sendUpdates "SPIN Search" (MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" (TypedText (tshow spin) :: AppianPickerData Text)))) val
  (ident :: AppianInt) <- handleMissing "SPIN Picker" v $ v ^? getPickerWidget "New Service Provider Information Number (SPIN)" . pwSuggestions . traverse . plate . key "identifier" . _JSON
  sendUpdates "Select SPIN" (MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" (Identifiers [ident])))) v

selectOldSPIN :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m) => Value -> AppianT m Value
selectOldSPIN v = do
  spin <- handleMissing "SPIN Column" v $ v ^? getGridFieldCell . traverse . gfColumns . at "SPIN" . traverse . _TextCell . traverse . _Just
  sendUpdates "Search for FRNs with SPIN" (MonadicFold (to (textUpdate "SPIN" spin))
                                   <|> MonadicFold (to (buttonUpdate "Search"))
                                   ) v
  return v
