{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Scripts.FCCForm471
  ( module Scripts.FCCForm471
  , module Scripts.FCCForm471Types
  ) where

import ClassyPrelude hiding (take, takeWhile)
import Control.Lens hiding (index, cons, snoc)
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Appian.Internal.Updates
import Data.Attoparsec.Text hiding (Result)
import Data.Time (addDays)
import Appian.Internal.Arbitrary
import qualified Test.QuickCheck as QC
import Control.Lens.Action
import Control.Lens.Action.Reified
import Scripts.Common
import qualified Data.Foldable as F
import qualified Data.Csv as Csv
import Control.Monad.Logger
import Control.Monad.Time
import Scripts.FCCForm471Types
import Scripts.FCCForm471Common (Form471Num(..))
import Data.Random (MonadRandom)
import Control.Retry
import Control.Monad.Except hiding (foldM, mapM_)

form471Intake :: (RapidFire m, MonadGen m, MonadIO m) => Form471Conf -> AppianT m Form471Num
form471Intake conf = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport rid
  form471Link <- handleMissing "FCC Form 471" v' $ v' ^? landingPageLink "FCC Form 471"
  aid <- handleMissing "Action ID" v' $ parseActionId form471Link
  pid <- landingPageAction aid
  v'' <- ( do
             v <- landingPageActionEx pid
             v' <- handleMultipleEntitiesOld checkMultipleOrgs "Apply for Funding Now" "Organization ID" (conf ^. selectOrgMethod) v
             case v' ^? isWindowClose of
               Nothing -> return v'
               Just False -> return v'
               Just True -> sendUpdates "Window Close" (buttonUpdateNoCheckF "Continue") v'
               
         )

  membersPage <- sendUpdates "Nickname" (    MonadicFold (textFieldArbitrary "Please enter an application nickname here." 255)
               <|> buttonUpdateNoCheckF "Save & Continue"
              ) v''
    >>= sendUpdates "Contact Info" (    buttonUpdateNoCheckF "Yes"
                     <|> paragraphArbitraryUpdate "Enter Holiday Contact Information" 4000
                     <|> buttonUpdateNoCheckF "Save & Continue"
                    )
    >>= sendUpdates "Choose Category" (    buttonUpdateNoCheckF (conf ^. category . to tshow)
                     <|> buttonUpdateNoCheckF "Save & Continue"
                    )

  entityInformation <- case membersCompleted membersPage of
    True -> sendUpdates "Entity Members" (buttonUpdateNoCheckF "Save & Continue") membersPage
    False -> selectMembers membersPage

  frnList <- sendUpdates "View Entity Types" (buttonUpdateNoCheckF "Save & Continue") entityInformation
    >>= sendUpdates "View Discount Rates" (buttonUpdateNoCheckF "Save & Continue")
    >>= createFRN conf (conf ^. nFRNs) (conf ^. spin)
  val <- case conf ^. createFRNType of
    NewFRN -> forLineItems conf frnList
    CopyFRN _ -> clickThroughAllFRNLineItems frnList

  let asDynamicLink = id :: DynamicLink -> DynamicLink

  val' <- case conf ^. category of
            Cat2 -> sendUpdates "Click Cat2 Budget Link" (componentUpdateWithF "Could not find 'Cat2 Budget Link'" $ hasKeyValue "testLabel" ">> Click to View" . _JSON . to asDynamicLink) val
            Cat1 -> return val

  val'' <- case val ^? getButton "Continue" of
    Just _ -> sendUpdates "Click 'Continue'" (buttonUpdateF "Continue") val'
    Nothing -> return val'

  ifContinueToCertification val''

isMultipleEntities :: (Plated s, AsValue s) => Text -> Fold s Text
isMultipleEntities buttonLabel = hasKeyValue "label" buttonLabel . key "#t" . _String

checkMultipleOrgs :: Value -> MultiOrgStatus
checkMultipleOrgs v = case v ^? isMultipleEntities "Apply For Funding Now" of
    Nothing -> IsSingle
    Just "FormLayout" -> IsMultiple
    Just _ -> Failure "There seems to be some change in the 'Select Organization' page or perhaps it is different for this operation?"


selectMembers :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectMembers = selectMembers'
-- selectMembers v = do
--   (v', rGridField) <- validMemberCheckboxes v
--   case rGridField of
--     Error msg -> throwError $ MissingComponentError ("selectMembers: " <> pack msg, v')
--     Success gridField -> do
--       taskId <- getTaskId v'
--       let updates = toUpdate gridField
--       checked <- sendUpdate (taskUpdate taskId) $ mkUiUpdate [updates] v'

--       sendUpdates "Add Members" (buttonUpdateNoCheckF "Add") checked
--         >>= sendUpdates "Click Save & Continue" (buttonUpdateNoCheckF "Save & Continue")

validMemberCheckboxes :: (RapidFire m, MonadGen m) => Value -> AppianT m (Value, Result (GridField GridFieldCell))
validMemberCheckboxes v = do
  logDebugN "Getting valid member checkboxes."
  let mRefs = v ^.. getGridFieldRecordRefs "BEN Name" . traverse
  case mRefs of
    [] -> throwError $ BadUpdateError "There are no entity members!" (Just v)
    refs -> do
      discountRates <- mapM (\r -> (,) <$> pure r <*> viewDiscountRates r) refs
      taskId <- getTaskId v
      let validRefs = discountRates ^.. traverse . filtered (has $ _2 . to insufficientDiscountRate . only False) . _1
          updates = v ^.. to (buttonUpdate "No") . traverse
      v' <- sendUpdate (taskUpdate taskId) $ mkUiUpdate updates v
      let gf = v' ^. getGridField
          refs' = gf ^.. traverse . gfColumns . at "BEN Name" . traverse . _TextCellLink . _2 . traverse
          idents = gf ^.. traverse . gfIdentifiers . traverse . ifolded . ifiltered (\i _ -> i `elem` indices)
          indices = refs ^.. ifolded . filtered (\r -> r `elem` validRefs) . withIndex . _1
      return (v', _Success . gfSelection . _Just . _Selectable . gslSelected .~ idents $ gf)

selectMembers' :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectMembers' v = do
  assign appianValue v

  sendUpdates1 "Select 'No' for 'Is every member in this consortium included on this FCC Form 471?'" (buttonUpdateF "No")
  validMemberCheckboxes'
  sendUpdates1 "Click 'Add' to add members" (buttonUpdateF "Add")
  sendUpdates1 "Click 'Save & Continue' to 'Entity Information'" (buttonUpdateF "Save & Continue")

  use appianValue

validMemberCheckboxes' :: (RapidFire m, MonadGen m) => AppianT m ()
validMemberCheckboxes' = do
  forGridRows1_ sendUpdates (^. runFold ((,) <$> Fold getGridFieldIdfs <*> Fold getGridFieldRefs) . to (uncurry zip)) (MonadicFold $ getGridField . traverse) viewDiscountRates'
  pure ()

getGridFieldRefs :: Traversal' (GridField GridFieldCell) (Vector RecordRef)
getGridFieldRefs = gfColumns . at "BEN Name" . traverse . _TextCellLink . _2

getGridFieldIdfs :: Traversal' (GridField GridFieldCell) (Vector GridFieldIdent)
getGridFieldIdfs = gfIdentifiers . traverse

viewDiscountRates :: (RapidFire m, MonadGen m) => RecordRef -> AppianT m Value
viewDiscountRates rref = do
  dashState <- runDashboardByName rref "Discount Rate"

  execDashboardFormT (pure ()) dashState
  use appianValue

viewDiscountRates' :: (RapidFire m, MonadGen m) => (GridFieldIdent, RecordRef) -> GridField GridFieldCell -> AppianT m ()
viewDiscountRates' (identifier, rref) gf = do
  v <- use appianValue
  eDashState <- runDashboardByName' rref "Discount Rate"
  case eDashState of
    Left _ -> return ()
    Right dashState -> do
      discountRates <- execDashboardFormT (usesValue' (^.. getDiscountRate)) dashState
      assign appianValue v

      case discountRates of
        [] -> return ()
        discounts -> do
          mapM_ (logDebugN . tshow) $ discounts
          sendUpdates1 "Selecting Member Entity"
            (componentUpdateWithF "The impossible happened! Check the viewDiscountRates function in 471 intake."
             (to (const $ gridFieldSelect identifier gf))
            )
        

gridFieldSelect :: GridFieldIdent -> GridField GridFieldCell -> GridField GridFieldCell
gridFieldSelect ident = gfSelection . traverse . _Selectable . gslSelected %~ (cons ident)

usesValue' :: Monad m => (Value -> a) -> DashboardFormT m a
usesValue' f = mkDashboardFormT $ \dashState -> do
  a <- usesValue f
  pure (a, dashState)

-- viewDiscountRates :: (RapidFire m, MonadGen m) => RecordRef -> AppianT m Value
-- viewDiscountRates rref = do
--   v <- viewRecordDashboard rref (Dashboard "summary")
--   dashboard <- handleMissing "Discount Rate" v $ v ^? getRecordDashboard "Discount Rate"
--   viewRecordDashboard rref dashboard

hasDiscountRate :: Value -> Bool
hasDiscountRate = has getDiscountRate

insufficientDiscountRate :: Value -> Bool
insufficientDiscountRate v = any (checkResult . parseResult) msgs
  where
    msgs = v ^.. hasKeyValue "name" "x-embedded-summary" . key "children" . plate . _String . _JSON . asValue . cosmos . key "validations" . plate . key "message" . _String
    parseResult = parseOnly (manyTill anyChar (string "not sufficient") *> manyTill anyChar (string "Discount Rate") *> pure True)
    checkResult (Left _) = False
    checkResult (Right b) = b

isWindowClose :: (Contravariant f, Applicative f, AsValue t) => (Bool -> f Bool) -> t -> f t
isWindowClose = key "title" . _String . to (=="FCC Form 471 Funding Year Window is Closed")

selectFirst :: Applicative f =>
     (GridFieldIdent -> f GridFieldIdent) -> GridField a0 -> f (GridField a0)
selectFirst = gfIdentifiers . traverse . taking 1 traverse

arbTextInt :: MonadGen m => LineItemSize -> m Text
arbTextInt Small = genArbitrary (resize 1 $ QC.arbitrarySizedNatural :: Gen Int) >>= pure . tshow . (+1)
arbTextInt Regular = genArbitrary (QC.arbitrarySizedNatural :: Gen Int) >>= pure . tshow . (+1)
arbTextInt Large = genArbitrary (resize 1000000 $ QC.arbitrarySizedNatural :: Gen Int) >>= pure . tshow . (+1)
arbTextInt (Range low high) = genArbitrary (QC.choose (low, high) :: Gen Int) >>= pure . tshow

membersCompleted :: Value -> Bool
membersCompleted v = case v ^? deep (filtered $ has $ key "value" . _String . prefixed "We've completed this section of the form based on information from your applicant entity's profile.") of
  Nothing -> False
  Just _ -> True

selectConsortiumMembers :: Value -> Bool
selectConsortiumMembers v = case v ^? getButton "Select an Entity by Consortium Members" of
  Nothing -> False
  Just _ -> True

selectEntities :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectEntities v = do
  v' <- case v ^? getButton "Manage Recipients of Service" of
    Nothing -> return v
    Just _ -> sendUpdates "Click 'Manage Recipients of Service'" (buttonUpdateF "Manage Recipients of Service") v

  case selectConsortiumMembers v' of
    True -> selectEntitiesReceivingService v'
    False -> sendUpdates "Manage Recipients of Service" (MonadicFold (getButtonWith (== "Yes") . to toUpdate . to Right)
                                                         <|> buttonUpdateNoCheckF "Save & Continue"
                                                        ) v'

selectEntitiesReceivingService :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectEntitiesReceivingService v
      = sendUpdates "Select Consortia Members" (buttonUpdateNoCheckF "Select an Entity by Consortium Members") v
    >>= foldDropdown "Select Consortium Member Entity Type"
                                         (\v -> case v ^? _JSON . to isEmpty470Grid of
                                             Just False -> sendUpdates "Add All Button" addAllButtonUpdate v
                                             _ -> return v
                                         ) "Search for Consortium Member by Entity Type"
    >>= isShared
    -- >>= sendUpdates "Select Consortium Member Entity Type" (MonadicFold (to (dropdownUpdate "Search for Consortium Member by Entity Type" 3)))
    -- >>= sendUpdates "Add All Dependent Schools and NIFs" (buttonUpdateNoCheckF "Add All Dependent Schools And NIFs ")))
    -- >>= sendUpdates "Manage Recipients of Service" (buttonUpdateNoCheckF "Save & Continue")))
    >>= sendUpdates "Manage Recipients of Service" (buttonUpdateWithF (\label -> label == "Save & Continue" || label == "Continue") "Could not locate 'Continue' button")

isShared :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
isShared v = case has (getTextField "Are the costs shared equally among all of the entities?") v of
  True -> sendUpdates "Select Yes (Equally Shared Costs)" (buttonUpdateNoCheckF "Yes"
                                                           <|> buttonUpdateNoCheckF "Save & Continue"
                                                          ) v
  False -> return v

isEmpty470Grid :: Value -> Bool
isEmpty470Grid v = case v ^? hasType "GridField" . key "numRowsToDisplay" . _Number . only 0.0 of
  Just _ -> True
  Nothing -> False

search470 :: (RapidFire m, MonadGen m) => Form470SearchType -> Value -> AppianT m Value
search470 (ByBEN ben) = sendUpdates "Search for 470 by BEN" (MonadicFold (to $ textUpdate "Search by BEN" ben)
                                                      <|> buttonUpdateNoCheckF "Search"
                                                     )
                        >=> select470
search470 (By470 form470ID) = sendUpdates "Search for 470 by 470 ID" (MonadicFold (to $ textUpdate "Search by FCC Form 470 Number" form470ID)
                                                                      <|> MonadicFold (to $ textUpdate "Search by BEN" mempty)
                                                                      <|> buttonUpdateNoCheckF "Search"
                                                                     )
                              >=> sendUpdates "Select 470 and Continue" (MonadicFold (to (gridFieldUpdate 0))
                                                                         <|> buttonUpdateNoCheckF "Continue"
                                                                        )
search470 No470 = sendUpdates "Click Search" (MonadicFold (to $ buttonUpdate "Search"))
                  >=> select470

select470 :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
select470 v = case isEmpty470Grid v of
  True -> sendUpdates "No Form 470" (buttonUpdateNoCheckF "No"
                                    <|> buttonUpdateNoCheckF "Continue"
                                    ) v
  False -> sendUpdates "Select 470 and Continue" (MonadicFold (to (gridFieldUpdate 0))
                             <|> buttonUpdateNoCheckF "Continue"
                            ) v

ifContinueToCertification :: (RapidFire m, MonadGen m) => Value -> AppianT m Form471Num
ifContinueToCertification v = do
  formNum <- handleMissing "Could not find 471 number!" v $ v ^? deep (key "title" . _String . to (parseOnly parse471Number) . traverse)
  case has (key "ui" . key "#t" . _String . only "FormLayout") v of
    True -> pure formNum
    False ->
      case v ^? getButton "Review FCC Form 471" of
        Just _ -> sendUpdates "Click Review FCC Form 471" (buttonUpdateNoCheckF "Review FCC Form 471") v
          >> pure formNum
        Nothing -> do
          v' <- sendUpdates "Click Continue to Certification" (buttonUpdateNoCheckF "Continue to Certification") v
          case v' ^? getButton "Review FCC Form 471" of
            Nothing -> pure formNum
            Just _ -> sendUpdates "Click Review FCC Form 471" (buttonUpdateNoCheckF "Review FCC Form 471") v'
              >> pure formNum

    -- Discards the result of f
foldDropdown :: (RapidFire m, MonadGen m) => Text -> (Value -> AppianT m Value) -> Text -> Value -> AppianT m Value
foldDropdown label f dfLabel val = foldM go Null indices
  where
    go _ n = do
      v <- sendUpdates (label <> ": Dropdown choice: " <> tshow n) (dfUpdate n) val
      _ <- f v
      return val
    indices :: [Int]
    indices = case val ^? getDropdown dfLabel of
      Just df -> [2.. (df ^. dfChoices . to length)]
      Nothing -> []
    dfUpdate n = MonadicFold (to (dropdownUpdate dfLabel n))

addAllButtonUpdate :: (Plated s, AsValue s, AsJSON s) => ReifiedMonadicFold m s (Either Text Update)
addAllButtonUpdate = MonadicFold (failing (getButtonWith getAddAllButton . to toUpdate . to Right) (to (const $ Left "Could not find the Add All button!")))

getAddAllButton :: Text -> Bool
getAddAllButton label
  =  label == "Add All Dependent Schools And NIFs "
  || label == "Add All Schools and Dependent NIFs "
  || label == "Add All Libraries and Dependent NIFs "
  || label == "Add All Dependent Libraries and NIFs "
  || label == "Add All Schools "
  || label == "Add All Libraries "

createFRN :: (RapidFire m, MonadGen m, MonadIO m) => Form471Conf -> Int -> Text -> Value -> AppianT m Value
createFRN _ 0 _ val = return val
createFRN conf n spin val = do
  logDebugN $ "Creating FRN with " <> tshow (n - 1) <> " to go."
  val' <- sendUpdates "Create new FRN" (MonadicFold $ to (buttonUpdate "Add FRN")) val
  frnList <- case conf ^. createFRNType of
    NewFRN -> createNewFRN conf spin val'
    CopyFRN method -> sendUpdates "Funding Request Key Information" (MonadicFold (textFieldArbitrary "Please enter a Funding Request Nickname here" 255)
                                                                     <|> MonadicFold (to $ buttonUpdate "No")
                                                                     <|> MonadicFold (to $ buttonUpdate "Copy FRN")
                                                                    ) val'
                      >>= copyFRN conf method
                      >>= sendUpdates "Select an FRN & Continue" (gridFieldArbitrarySelect
                                                                  <|> MonadicFold (to $ buttonUpdate "Continue")
                                                                 )
                      >>= (\v -> do
                              assign appianValue v
                              retrying refreshRetryPolicy retryRefresh (const $ do
                                                                           sendUpdates1 "Select refresh" (MonadicFold $ to $ buttonUpdate "Refresh" )
                                                                           use appianValue
                                                                       )
                          )
                      >>= sendUpdates "Continue to Key Information" (MonadicFold $ to $ buttonUpdate "Continue")
                      >>= sendUpdates "Continue to Contract" (MonadicFold $ to $ buttonUpdate "Continue")
                      >>= sendUpdates "Continue to Contract Dates" (MonadicFold $ to $ buttonUpdate "Continue")
                      -- The below may change based on contract type
                      >>= setDates
                      >>= sendUpdates "Save Narrative & Continue" (MonadicFold (to $ buttonUpdate "Save & Continue"))

  createFRN conf (n - 1) spin frnList

refreshRetryPolicy :: RetryPolicy
refreshRetryPolicy = exponentialBackoff 8000000 `mappend` limitRetries 5

retryRefresh :: MonadIO m => RetryStatus -> Value -> m Bool
retryRefresh _ v = pure $ not $ has (deep $ hasKeyValue "#v" "FRN has been successfully copied.") v

copyFRN :: (RapidFire m, MonadGen m) => Form471Conf -> SearchFRNMethod -> Value -> AppianT m Value
copyFRN conf (ByFRNNumber num) = sendUpdates "Search by FRN Number" (MonadicFold (to $ textUpdate "Search by FRN Number" $ tshow num)
                                                                     <|> MonadicFold (to $ buttonUpdate "Search")
                                                                    )
copyFRN conf (ByFCCForm471 num) = sendUpdates "Search by FCC Form 471" (MonadicFold (to $ textUpdate "Search by FCC Form 471" $ tshow num)
                                                                        <|> MonadicFold (to $ buttonUpdate "Search")
                                                                       )

createNewFRN :: (RapidFire m, MonadGen m) => Form471Conf -> Text -> Value -> AppianT m Value
createNewFRN conf spin val = do
  dates <- sendUpdates "Funding Request Key Information" (    MonadicFold (textFieldArbitrary "Please enter a Funding Request Nickname here" 255)
                                               <|> buttonUpdateNoCheckF "No"
                                               <|> MonadicFold (to (dropdownUpdate "Category 1 Service Types" 2))
                                               <|> buttonUpdateNoCheckF "Continue"
                                              ) val
            >>= sendUpdates "Select Purchase Type" (buttonUpdateNoCheckF "Tariff"
                              <|> buttonUpdateNoCheckF "Continue"
                                                   )
            >>= sendUpdates "Enter number of Bids" ( MonadicFold (intFieldArbitrary "How many bids were received?") -- (to (textUpdate "How many bids were received?" "3"))
                                     <|> buttonUpdateNoCheckF "Yes"
                                   )
            >>= search470 (conf ^. form470Search)
--             >>= select470
            >>= sendUpdates "SPIN Search" (    MonadicFold (to (textUpdate "Search by SPIN" spin))
                             <|> buttonUpdateNoCheckF "Search"
                            )
            >>= sendUpdates "Select SPIN and Continue" ( MonadicFold (to (gridFieldUpdate 0))
                            <|> buttonUpdateNoCheckF "Continue"
                            )

  startDate <- handleMissing "Start Date" dates $ dates ^? hasLabel "What is the service start date?" . dpwValue . appianDate . traverse

  pricing <- sendUpdates "Enter funding dates and Continue" (MonadicFold (to (datePickerUpdate "When will the services end? " (AppianDate $ Just $ addDays 360 startDate)))
              <|> buttonUpdateNoCheckF "Continue"
              ) dates

  res <- sendUpdates "Price Confidentiality and Continue" (buttonUpdateNoCheckF "No"
               <|> buttonUpdateNoCheckF "Continue"
              ) pricing

  narrative <- case res ^? hasKeyValue "label" "Fiber Request Key Information" of
    Nothing -> return res
    Just _ -> sendUpdates "Fiber Request Key Information" (MonadicFold (to $ buttonUpdate "No")
                                                          <|> MonadicFold (to $ buttonUpdate "Continue")
                                                          ) res

  sendUpdates "Enter narrative and Continue" (paragraphArbitraryUpdate "Provide a brief explanation of the products and services that you are requesting, or provide any other relevant information regarding this Funding Request. You should also use this field to describe any updates to your entity data, such as revised student counts, entity relationships, etc, that you were unable to make after the close of the Administrative filing window for profile updates. These changes will be addressed during the application review process." 4000
                           <|> buttonUpdateNoCheckF "Save & Continue"
                         ) narrative

setDates :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
setDates v = do
  today <- utctDay <$> currentTime
  sendUpdates "Enter funding dates and Continue" (MonadicFold (to (datePickerUpdate "What is the service start date?" (AppianDate $ Just today)))
                                                  <|> MonadicFold (to (datePickerUpdate "What is the date your contract expires for the current term of the contract?" (AppianDate $ Just $ addDays 360 today)))
                                                  <|> buttonUpdateNoCheckF "Continue"
                                                 ) v

forLineItems :: (RapidFire m, MonadGen m) => Form471Conf -> Value -> AppianT m Value
forLineItems conf = forGridRows_ sendUpdates (^. gfColumns . at "FRN" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> addLineItem' conf dyl v)

addLineItem' :: (RapidFire m, MonadGen m) => Form471Conf -> DynamicLink -> Value -> AppianT m Value
addLineItem' conf dyl v = sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= addLineItem'' (conf ^. nLineItems)
  -- >>= sendUpdates "Review FRN Line Items" (buttonUpdateWithF (\label -> label == "Continue" || label == "Review FCC Form 471") "Could not locate 'Continue' or 'Review 471 Button'")
  >>= toFRNGrid 
  where
    addLineItem'' 0 val = return val
    addLineItem'' n val = do
      logDebugN $ "Creating line item " <> tshow (conf ^. nLineItems - n + 1)
      sendUpdates "Add New FRN Line Item" (buttonUpdateNoCheckF "Add New FRN Line Item") val
        >>= selectFunction
        >>= handleDataQuestions
        >>= enterCosts conf
        >>= selectEntities
        >>= sendUpdates "Review Recpients" (buttonUpdateNoCheckF "Continue")
--        >>= sendUpdates "Select the sub-category" (dropdownUpdateF1 "Select the sub-category you want to modify" "Funding Request Details")
--        >>= sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right))
        >>= (\v -> addLineItem'' (n - 1) v)
    toFRNGrid val = case val ^? getDropdown "Select the sub-category you want to modify" of
      Nothing -> sendUpdates "Review FRN Line Items" (buttonUpdateWithF (\label -> label == "Continue") "Could not locate 'Continue' Button") val
      Just _ -> sendUpdates "Select 'Funding Request Details'" (dropdownUpdateF1 "Select the sub-category you want to modify" "Funding Request Details") val

selectFunction :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
selectFunction v = do
  assign appianValue v
  hasFunction <- usesValue (has $ getDropdown "Function")

  case hasFunction of
    True -> do
      selectPurpose
      sendUpdates1 "Select Function" (dropdownArbitraryUpdateF "Function")
      sendUpdates1 "Select 'Type of Connection'" (dropdownArbitraryUpdateF "Type of Connection")
      enterOther
      sendUpdates1 "Click 'Continue' from Function Details" (buttonUpdateF "Continue")
    False -> do
      sendUpdates1 "Total Quantity of Equipment Maintained" (intFieldArbitraryUpdateF "Total Quantity of Equipment Maintained")
      sendUpdates1 "Click 'Continue' from Function Details" (buttonUpdateF "Continue")

  use appianValue
  -- case has (getDropdown "Function") v of
  --   True -> sendUpdates "Select Function" (dropdownArbitraryUpdateF "Function") v
  --           >>= sendUpdates "Select Type of Connection and Continue" (dropdownArbitraryUpdateF "Type of Connection" -- MonadicFold (to (dropdownUpdate "Type of Connection" 13))
  --                                                                     <|> MonadicFold (radioButtonUpdate "Purpose" 1)
  --                                                                     <|> buttonUpdateNoCheckF "Continue"
  --                                                                    )
  --   -- False -> sendUpdates "Select Type of Connection" (MonadicFold (to $ dropdownUpdate "Type of Internal Connection" 2)) v
  --   --   >>= sendUpdates "Select Type, Make, Model, and Continue" (MonadicFold (to $ dropdownUpdate "Type of Product" 2)
  --   --                                                            <|> dropdownArbitraryUpdateF "Make"
  --   --     						       <|> MonadicFold (textFieldArbitrary "Enter the Make" 255)
  --   --                                                            <|> MonadicFold (textFieldArbitrary "Model" 255)
  --   --                                                            <|> MonadicFold (getButtonWith (== "Yes") . to toUpdate . to Right)
  --   --                                                            <|> MonadicFold (to $ buttonUpdate "Continue")
  --   --                                                            )
  --   False -> sendUpdates "Total Quantity of Equipment Maintained" (MonadicFold (to $ textUpdate "Total Quantity of Equipment Maintained" "2")
  --                                                                  <|> buttonUpdateNoCheckF "Continue"
  --                                                                 ) v

selectPurpose :: (RapidFire m, MonadGen m) => AppianT m ()
selectPurpose = do
  hasPurpose <- usesValue (has $ getRadioButtonField "Purpose")
  case hasPurpose of
    True -> sendUpdates1 "Select 'Purpose'" (radioArbitraryF "Purpose")
    False -> return ()

enterOther :: (RapidFire m, MonadGen m) => AppianT m ()
enterOther = do
  hasOther <- usesValue (has $ getTextField "Enter Type of Connection, if Other was selected")
  case hasOther of
    True -> sendUpdates1 "Enter 'Other'" (textFieldArbitraryF "Enter Type of Connection, if Other was selected" 255)
    False -> return ()

handleDataQuestions :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
handleDataQuestions v = do
  case v ^? hasKeyValue "value" "Please enter Bandwidth Speed Information for this Data Transmission and/or Internet Access Line Item" of
    Nothing -> return $ trace "No bandwidth questions" v
    Just _ -> do
      assign appianValue v

      bdw <- genArbitrary arbitrary
      enterBandwidthSpeeds bdw

      -- burstable <- genArbitrary arbitrary
      -- enterInput (burstable :: Burstable)

      sendUpdates1 "Click 'Continue'" (buttonUpdateF "Continue")

      use appianValue
        >>= handleConnections

-- handleDataQuestions v = do
--   logDebugN "Handling Data Questions"
--   case v ^? hasKeyValue "value" "Please enter Bandwidth Speed Information for this Data Transmission and/or Internet Access Line Item" of
--     Nothing -> return $ trace "No bandwidth questions" v
--     Just _ -> sendUpdates "Not Burstable & Continue" (buttonUpdateNoCheckF "No"
--                                                      <|> buttonUpdateNoCheckF "Continue"
--                                                      ) v
--                 >>= handleConnections

enterBandwidthSpeeds :: RapidFire m => BandwidthSpeeds -> AppianT m ()
enterBandwidthSpeeds bdw = enterInput bdw

handleConnections :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
handleConnections v = do
  case has (getDropdown "Connection used by") v of
    True -> sendUpdates "Select All No & Continue" (MonadicFold (getButtonWith (\l -> l == "No" || l == "No ✓") . to toUpdate . to Right)
                                                     <|> MonadicFold (to (dropdownUpdate "Connection used by" 2))
                                                     <|> buttonUpdateNoCheckF "Continue"
                                                   ) v
    False -> sendUpdates "Select All No & Continue" (MonadicFold (getButtonWith (\l -> l == "No" || l == "No ✓") . to toUpdate . to Right)
                                                     <|> buttonUpdateNoCheckF "Continue"
                                                   ) v

arbLineItemCost :: LineItemSize -> QC.Gen LineItemCost
arbLineItemCost Small = resize 1 QC.arbitrary
arbLineItemCost Regular = QC.arbitrary
arbLineItemCost Large = resize 1000000 QC.arbitrary
arbLineItemCost (Range low high) = QC.scale (const high) QC.arbitrary

enterCosts :: (RapidFire m, MonadGen m) => Form471Conf -> Value -> AppianT m Value
enterCosts conf v = do
  assign appianValue v

  lineItemCost <- genArbitrary $ arbLineItemCost $ conf ^. lineItemSize
  enterLineItemCost lineItemCost

  sendUpdates1 "Click 'Save & Continue'" (buttonUpdateF "Save & Continue")

  use appianValue
-- enterCosts conf v =
--   case conf ^. category of
--     Cat1 -> case has (hasKeyValue "_cId" "d2d5fc7028c296c76a2a9698ed79bd69") v of
--               False -> sendUpdates "Enter Cost Calculations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                             <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
--                                                                             <|> MonadicFold (act (\v -> textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                             <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                             <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
--                                                                             <|> MonadicFold (act (\v -> textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                             <|> buttonUpdateNoCheckF "Save & Continue"
--                                                                           ) v
--               True -> sendUpdates "Enter Cost Calculations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
--                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
--                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                            <|> dropdownCidArbitraryUpdateF "d2d5fc7028c296c76a2a9698ed79bd69"
--                                                                            <|> buttonUpdateNoCheckF "Save & Continue"
--                                                                          ) v
--     Cat2 -> sendUpdates "Enter Costs Calutations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
--                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
--                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
--                                                                <|> buttonUpdateNoCheckF "Save & Continue"
--                                                                ) v

enterLineItemCost :: RapidFire m => LineItemCost -> AppianT m ()
enterLineItemCost = enterInput

parse471Number :: Parser Form471Num
parse471Number = string "Create FCC Form 471 - " *> (Form471Num <$> decimal)
-- parse471Number = manyTill anyChar (string " - Form # ") *> (Form471Num <$> decimal)

clickThroughAllFRNLineItems :: (RapidFire m, MonadGen m) => Value -> AppianT m Value
clickThroughAllFRNLineItems val = forGridRows_ sendUpdates (^. gfColumns . at "FRN" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> clickThroughFRNLineItems dyl v) val

clickThroughFRNLineItems :: (RapidFire m, MonadGen m) => DynamicLink -> Value -> AppianT m Value
clickThroughFRNLineItems dyl v = sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= forGridRows_ sendUpdates (^. gfColumns . at "FRN Line Item Number" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> clickThroughLineItem dyl v)
  >>= sendUpdates "Continue to FRN Summary" (MonadicFold $ to $ buttonUpdate "Continue")

clickThroughLineItem :: (RapidFire m, MonadGen m) => DynamicLink -> Value -> AppianT m Value
clickThroughLineItem dyl v = sendUpdates "Click Line Item" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= sendUpdates "Continue to Cost Calculation" (MonadicFold $ to $ buttonUpdate "Continue")
  >>= sendUpdates "Continue to Recipients of Service" (MonadicFold $ to $ buttonUpdate "Save & Continue")
  >>= sendUpdates "Continue to Line Item Summary" (MonadicFold $ to $ buttonUpdate "Continue")
