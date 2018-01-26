{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Scripts.FCCForm471EditApplication
  ( runEdit471Application
  ) where

import ClassyPrelude
import Appian.Client
import Appian
import Appian.Instances
import Appian.Lens
import Appian.Types
import Data.Aeson.Encode.Pretty
import Scripts.Common
import Scripts.Execute
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action.Reified
import Stats.CsvStream
import qualified Data.Csv as Csv
import Scripts.FCCForm471ReviewAssignment
import Scripts.FCCForm471Types
import Scripts.FCCForm471 (forLineItems)
import Appian.Internal.Arbitrary

data EditApplicationMode
    = WindowVersion
    | DisasterReliefVersion
    | RemoveChildEntityVersion
    | AddLineItem
    | RemoveLineItem
    | AddFRN
    | RemoveFRN
    deriving (Show, Eq)

instance Csv.FromField EditApplicationMode where
    parseField bs = case decodeUtf8 bs of
        "window" -> pure WindowVersion
        "disaster" -> pure DisasterReliefVersion
        "rmChildEnity" -> pure RemoveChildEntityVersion
        "addLineItem" -> pure AddLineItem
        "rmLineItem" -> pure RemoveLineItem
        "addFRN" -> pure AddFRN
        "rmFRN" -> pure RemoveFRN
        _ -> fail $ "Invalid Edit Application Mode" <> show bs

data EditAppConfig = EditAppConfig
    { _editAppMode :: EditApplicationMode
    , _form471ReviewConf :: Form471ReviewConf
    , _form471Conf :: Form471Conf
    } deriving Show

makeLenses ''EditAppConfig

instance Csv.FromNamedRecord EditAppConfig where
    parseNamedRecord bs = EditAppConfig
        <$> bs Csv..: "Edit Mode"
        <*> Csv.parseNamedRecord bs
        <*> Csv.parseNamedRecord bs
        
instance HasLogin EditAppConfig where
    getLogin conf = getLogin $ conf ^. form471ReviewConf

removeLineItem :: RapidFire m => AppianT m ()
removeLineItem = do
    sendUpdates1 "APPLICATION" (buttonUpdateF "Application")
    sendUpdates1 "Select the sub-category you want to modify" (dropdownUpdateF1 "Select the sub-category you want to modify" "Funding Request Details")
    sendUpdates1 "Select FRN Dynamic Link" (gridDynamicLinkUpdateF "FRN" 3)
    sendUpdates1 "Select FRN Line Item in grid" (gridFieldUpdateWithF (dropping 1 getGridFieldCell) 0)
    sendUpdates1 "Click Delete Line Items button" (buttonUpdateWithF isDeleteAllButton "Could not find Delete Line Items button")
    sendUpdates1 "Click Finish button" (buttonUpdateF "Finish")
    
removeFRN :: RapidFire m => AppianT m ()
removeFRN = do
    sendUpdates1 "APPLICATION" (buttonUpdateF "Application")
    sendUpdates1 "Select the sub-category you want to modify" (dropdownUpdateF1 "Select the sub-category you want to modify" "Funding Request Details")
    sendUpdates1 "Select FRN in grid" (gridFieldUpdateWithF getGridFieldCell 3)
    sendUpdates1 "Click Remove FRN button" (buttonUpdateF "Remove FRN")
    sendUpdates1 "Click Finish button" (buttonUpdateF "Finish")
    
editHolidayContact :: RapidFire m => AppianT m ()
editHolidayContact = do
    sendUpdates1 "APPLICATION" (MonadicFold $ to $ buttonUpdate "Application")
    sendUpdates1 "Select the sub-category you want to modify" (dropdownUpdateF1 "Select the sub-category you want to modify" "Application Details")
    sendUpdates1 "Click Continue button" (buttonUpdateF "Continue")
    sendUpdates1 "Click Edit Application" (buttonUpdateF "Edit Application")
    sendUpdates1 "Enter Holiday Contact Info" (paragraphUpdateF "Enter Holiday Contact Information" "Update to new Folds is working!")
    sendUpdates1 "Click Continue button" (buttonUpdateF "Continue")
    sendUpdates1 "Click Finish button" (buttonUpdateF "Finish")
    
editDisaster :: RapidFire m => AppianT m ()
editDisaster = do
    sendUpdates1 "Select Entity" (MonadicFold $ to $ buttonUpdate "Entity")
    sendUpdates1 "Select BEN" (MonadicFold $ to $ buttonUpdate "BEN") 
    sendUpdates1 "Select BEN name in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    sendUpdates1 "Select manage organization relationships" (buttonUpdateF "Manage Organization Relationships")
    sendUpdates1 "Select Add a Child Entity" (buttonUpdateWithF isAddChildButton "Could not find button Add a Child Entity")
    sendUpdates1 "Select an organization type" (dropdownUpdateF1 "Select an Organization Type" "School")
    sendUpdates1 "Enter Name search" (textUpdateF "Name Search" "a")
    sendUpdates1 "Select State Search" (dropdownUpdateF1 "State Search" "Please select a value")
    sendUpdates1 "Click Search button" (buttonUpdateF "Search")
    sendUpdates1 "Select School from grid" (gridFieldUpdateWithF (dropping 1 getGridFieldCell) 0)
    sendUpdates1 "select First as YES" (componentUpdateWithF "Could not find YES/NO button" $ taking 1 $ getButton "Yes")
    sendUpdates1 "Click Submit button" (buttonUpdateF "Submit")
    sendUpdates1 "Click Finish button" (buttonUpdateF "Finish")
    
removeChildEntity :: RapidFire m => AppianT m ()
removeChildEntity = do
    sendUpdates1 "Select Entity" (MonadicFold $ to $ buttonUpdate "Entity")
    sendUpdates1 "Select BEN" (MonadicFold $ to $ buttonUpdate "BEN") 
    sendUpdates1 "Select BEN name in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    sendUpdates1 "Select manage organization relationships" (buttonUpdateF "Manage Organization Relationships")
    sendUpdates1 "Click Remove a Relationship button" (buttonUpdateF "Remove a Relationship")
    sendUpdates1 "Enter Name search" (textUpdateF "Name Search" "a")
    sendUpdates1 "Select State Search" (dropdownUpdateF1 "State Search" "Please select a value")
    sendUpdates1 "Click Search button" (buttonUpdateF "Search")
    sendUpdates1 "Select BEN/CRN in grid" (gridFieldUpdateWithF getGridFieldCell 0)
    sendUpdates1 "Click Submit button" (buttonUpdateF "Submit")
    sendUpdates1 "Click Finish button" (buttonUpdateF "Finish")

isAddChildButton :: Text -> Bool
isAddChildButton "Add a Child Entity" = True
isAddChildButton "Add Consortium Member" = True
isAddChildButton _ = False

addLineItem :: (RapidFire m, MonadGen m) => Form471Conf -> AppianT m ()
addLineItem conf = do
    sendUpdates1 "APPLICATION" (buttonUpdateF "Application")
    sendUpdates1 "Select the sub-category you want to modify" (dropdownUpdateF1 "Select the sub-category you want to modify" "Funding Request Details")
    addLineItemToAllFRNs conf
    -- sendUpdates1 "Select FRN Dynamic Link" (gridDynamicLinkUpdateF "FRN" 0)
    -- sendUpdates1 "Click 'Add New FRN Line Item'" (buttonUpdateF "Add New FRN Line Item")

addLineItemToAllFRNs :: (RapidFire m, MonadGen m) => Form471Conf -> AppianT m ()
addLineItemToAllFRNs conf = do
  v <- use appianValue
  forLineItems conf v
  assign appianValue v

-- | This switch will take the "Edit Mode" column of the .csv file.
-- Depending on what mode is present on that row this function will
editAppSwitch :: (RapidFire m, MonadGen m) => Form471Conf -> EditApplicationMode -> AppianT m ()
editAppSwitch _ RemoveLineItem = removeLineItem
editAppSwitch _ RemoveFRN = removeFRN
editAppSwitch conf AddLineItem = addLineItem conf
editAppSwitch _ WindowVersion = editHolidayContact
editAppSwitch _ DisasterReliefVersion = editDisaster
editAppSwitch _ RemoveChildEntityVersion = removeChildEntity

-- | Utility function for selecting a specific row in a specific column.
-- The cell must be a Text Cell with a Dynamic Link otherwise it will fail.
-- This will work for the EPC-16793 scenarios in the EPC 17.7.3 minor release.
gridDynamicLinkUpdateF :: Text -> Int -> ReifiedMonadicFold m Value (Either Text Update)
gridDynamicLinkUpdateF columnName rowNum = componentUpdateWithF "Could not find row" $ selectGridDynamicLink columnName rowNum

selectGridDynamicLink :: Text -> Int -> Fold Value DynamicLink
selectGridDynamicLink columnName rowNum 
    = getGridFieldCell
        . traverse
        . gfColumns
        . at columnName
        . traverse
        . _TextCellDynLink
        . _2
        . ix rowNum
        
isDeleteAllButton :: Text -> Bool
isDeleteAllButton label = isPrefixOf "Delete" label && isSuffixOf "Line Item(s)" label

edit471Application :: (RapidFire m, MonadGen m) => EditAppConfig -> AppianT m Value
edit471Application conf = do
    myAssigned471AppReport (conf ^. form471ReviewConf)
    clickApplication "Edit Application"
    editAppSwitch (conf ^. form471Conf) (conf ^. editAppMode)
    use appianValue

runEdit471Application :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runEdit471Application = runIt edit471Application
