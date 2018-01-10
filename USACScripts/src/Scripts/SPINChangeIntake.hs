{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Data.Csv as Csv
import Data.Random (MonadRandom)
import Control.Monad.Except

data SpinChangeType
  = SpinChangeBulk
  | SpinChangeGlobal
  | SpinChange
  | SpinChangeSRC
  deriving (Show, Read, Eq)

instance Parseable SpinChangeType where
  parseElement "Bulk SPIN" = return SpinChangeBulk
  parseElement "Global SPIN" = return SpinChangeGlobal
  parseElement "SRC SPIN" = return SpinChangeSRC
  parseElement "SPIN Change" = return SpinChange
  parseElement txt = Left $ tshow txt <> " is not a recognized SPIN Change Type." -- throwM $ ParseException $ tshow txt <> " is not a recognized SPIN Change Type."

data SpinChangeConfig = SpinChangeConfig
  { _spinChangeType :: SpinChangeType
  , _spinChangeCreator :: Login
  } deriving Show

makeLenses ''SpinChangeConfig

instance Csv.FromNamedRecord SpinChangeConfig where
  parseNamedRecord r = SpinChangeConfig
    <$> r Csv..: "spinChangeType"
    <*> Csv.parseNamedRecord r

instance Csv.FromField SpinChangeType where
  parseField bs = case parseElement (decodeUtf8 bs) of
    Left err -> fail $ show err
    Right v -> return v

instance HasLogin SpinChangeConfig where
  getLogin conf = conf ^. spinChangeCreator

spinChangeIntake :: (RapidFire m, MonadGen m) => SpinChangeConfig -> AppianT m (Maybe Text)
spinChangeIntake conf = do
  v <- myLandingPageAction "SPIN Change"

  spinChangeIntake' conf v

spinChangeIntake' :: (RapidFire m, MonadGen m) => SpinChangeConfig -> Value -> AppianT m (Maybe Text)
spinChangeIntake' conf v = do
  let un = Identifiers [AppianUsername "kyle.davie@fwisd.org"]

  v' <- case v ^? hasKeyValue "label" "Existing Organizations" of
    Nothing -> return v
    Just _ -> sendUpdates "Select Organization" ( gridFieldArbitrarySelect
                                                <|> MonadicFold (to (buttonUpdate "Create SPIN Change"))
                                                ) v

  sendUpdates "Nickname & SPIN Change Type" (MonadicFold (textFieldArbitrary "Nickname" 255)
                                             <|> dropdownUpdateF' "SPIN Change Type" (conf ^. spinChangeType)
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

printFRNs :: (RapidFire m, MonadGen m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
printFRNs val gf = do
  v <- foldGridField (printFRN val) "FRN" val gf
  return (v, v)

printFRN :: (RapidFire m, MonadGen m) => Value -> Value -> GridFieldCell -> AppianT m Value
printFRN val _ gf = (F.foldlM . F.foldlM) f val $ gf ^.. _TextCellDynLink . _2
  where
    f _ dyl =   sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) val
            >>= sendUpdates "No & Continue" ( MonadicFold (to $ buttonUpdate "No")
                                              <|> MonadicFold (to $ buttonUpdate "Continue")
                                            )

selectNewSPIN :: (RapidFire m, MonadGen m) => Int -> Value -> AppianT m Value
selectNewSPIN spin val = do
  v <- sendUpdates "SPIN Search" (MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" (TypedText (tshow spin) :: AppianPickerData Text)))) val
  (ident :: AppianInt) <- handleMissing "SPIN Picker" v $ v ^? getPickerWidget "New Service Provider Information Number (SPIN)" . pwSuggestions . traverse . plate . key "identifier" . _JSON
  sendUpdates "Select SPIN" (MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" (Identifiers [ident])))) v

selectOldSPIN :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectOldSPIN v = do
  spin <- handleMissing "SPIN Column" v $ v ^? getGridFieldCell . traverse . gfColumns . at "SPIN" . traverse . _TextCell . traverse . _Just
  sendUpdates "Search for FRNs with SPIN" (MonadicFold (to (textUpdate "SPIN" spin))
                                   <|> MonadicFold (to (buttonUpdate "Search"))
                                   ) v
  return v
