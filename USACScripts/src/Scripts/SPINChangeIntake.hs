{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Appian.Internal.Arbitrary
import Scripts.Common
import qualified Data.Foldable as F
import Control.Monad.Time
import qualified Data.Csv as Csv
import Data.Random (MonadRandom)
import Control.Monad.Except
import Scripts.Execute
import Stats.CsvStream

newtype SpinChangeType = SpinChangeType Text
  deriving (Show, Eq, Parseable)

newtype FY = FY { _unFY :: Int }
  deriving (Show, Eq, Parseable, Csv.FromField)

newtype NewSPIN = NewSPIN Int
  deriving (Show, Eq, Parseable, Csv.FromField)

data SpinChangeConfig = SpinChangeConfig
  { _spinChangeType :: SpinChangeType
  , _spinChangeCreator :: Login
  , _spinFundingYear :: FY
  , _spinNewSPIN :: NewSPIN
  } deriving Show

makeLenses ''SpinChangeConfig

instance Csv.FromNamedRecord SpinChangeConfig where
  parseNamedRecord r = SpinChangeConfig
    <$> r Csv..: "spinChangeType"
    <*> Csv.parseNamedRecord r
    <*> r Csv..: "fy"
    <*> r Csv..: "New Spin"

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

  assign appianValue v'
  sendUpdates1 "Enter 'Nickname'" (textFieldArbitraryF "Nickname" 255)
  sendUpdates1 "Select 'SPIN Change Type'" (dropdownUpdateF' "SPIN Change Type" (conf ^. spinChangeType))
  sendUpdates1 "Select 'Funding Year'" (dropdownUpdateF1 "Funding Year" (conf ^. spinFundingYear . to _unFY . to tshow . to DropdownValue))
  sendUpdates1 "Select 'Main Contact'" (pickerUpdateF "Main Contact Person" un)
  sendUpdates1Handle ignoreAssociateValidation "Click 'Continue'" (buttonUpdateF "Continue")

  selectOldSPIN
  sendUpdates1 "Click 'Add FRNs' button" (buttonUpdateWithF isAddAllButton "Could not find 'Add all FRNs' button")
  sendUpdates1 "Click 'Continue'" (buttonUpdateF "Continue")
  sendUpdates1 "Click 'Select SPIN Change Reason'" (dropdownArbitraryUpdateF "Please select the reason why you would like to change the service provider on the FRN(s)")

  use appianValue
    >>= selectNewSPIN (conf ^. spinNewSPIN)
    >>= \v -> foldGridFieldPages (MonadicFold (getGridFieldCell . traverse)) printFRNs v v
    >>= assign appianValue

  sendUpdates1 "Click 'Preview/Continue'" (paragraphArbitraryUpdateNoError "Other" 255
                                 <|> buttonUpdateWithF (\label -> trace (show label) label == "Preview" || label == "Continue") "Could not find 'Continue/Preview' button."
                                )
  v <- use appianValue
  case v ^? hasKeyValue "label" "Additional Questions" of
    Nothing -> return ()
    Just _ -> do
      sendUpdates1 "Click all 'No'" (componentUpdateWithF "Could not find 'No' buttons" $ getButton "No")
      sendUpdates1 "Click 'Preview'" (buttonUpdateF "Preview")

  sendUpdates1 "Click 'Submit'" (buttonUpdateF "Submit")

  use appianValue
    >>= \res -> return (res ^? deep (filtered $ has $ key "#v" . _String . suffixed "has been successfully created") . key "#v" . _String . to parseNumber . traverse)

printFRNs :: (RapidFire m, MonadGen m) => Value -> GridField GridFieldCell -> AppianT m (Value, Value)
printFRNs val gf = do
  v <- foldGridField (printFRN val) "FRN" val gf
  return (v, v)

printFRN :: (RapidFire m, MonadGen m) => Value -> Value -> GridFieldCell -> AppianT m Value
printFRN val _ gf = (F.foldlM . F.foldlM) f val $ gf ^.. _TextCellDynLink . _2
  where
    f _ dyl = do
      assign appianValue val
      sendUpdates1 "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right))
      sendUpdates1 "Click all 'No'" (componentUpdateWithF "Could not find 'No' buttons" $ getButton "No")
      sendUpdates1 "Click 'Continue'" (buttonUpdateF "Continue")
      
      use appianValue

selectNewSPIN :: (RapidFire m, MonadGen m) => NewSPIN -> Value -> AppianT m Value
selectNewSPIN (NewSPIN spin) val = do
  v <- sendUpdates "SPIN Search" (MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" (TypedText (tshow spin) :: AppianPickerData Text)))) val
  (ident :: AppianInt) <- handleMissing "Could not find SPIN Picker" v $ v ^? getPickerWidget "New Service Provider Information Number (SPIN)" . pwSuggestions . traverse . plate . key "identifier" . _JSON
  sendUpdates "Select SPIN" (MonadicFold (to (pickerUpdate "New Service Provider Information Number (SPIN)" (Identifiers [ident])))) v

selectOldSPIN :: (RapidFire m, MonadGen m) => AppianT m ()
selectOldSPIN = do
  v <- use appianValue
  spin <- handleMissing "SPIN Column" v $ v ^? getGridFieldCell . traverse . gfColumns . at "SPIN" . traverse . _TextCell . traverse . _Just
  sendUpdates1 "Search for FRNs with SPIN" (textUpdateF "SPIN" spin)
  sendUpdates1 "Click 'Search'" (buttonUpdateF "Search")
  
runSpinChangeIntake :: Bounds -> HostUrl -> LogMode -> CsvPath -> NThreads -> NumRecords -> IO ()
runSpinChangeIntake = runScriptExhaustive spinChangeIntake

-- | This function is needed because the button label changes depending on the number of FRNs available. This is equivalent to the regex 'Add all (.*) FRNs'
isAddAllButton :: Text -> Bool
isAddAllButton label = isPrefixOf "Add all (" label && isSuffixOf ") FRNs" label
