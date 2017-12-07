{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.FCCForm471
  ( module Scripts.FCCForm471
  , module Scripts.FCCForm471Types
  ) where

import ClassyPrelude hiding (take, takeWhile)
import Control.Lens hiding (index)
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Attoparsec.Text hiding (Result)
import Data.Time (addDays)
import Scripts.Test
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

form471Intake :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m, MonadIO m) => Form471Conf -> AppianT m Value
form471Intake conf = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport rid
  form471Link <- handleMissing "FCC Form 471" v' $ v' ^? landingPageLink "FCC Form 471"
  aid <- handleMissing "Action ID" v' $ parseActionId form471Link
  pid <- landingPageAction aid
  v'' <- ( do
             v <- landingPageActionEx pid
             v' <- case v ^? isMultipleEntities of
               Nothing -> return v
               Just "FormLayout" -> case conf ^. selectOrgMethod of
                 ByArbitrary -> sendUpdates "Apply for Funding Now" (gridFieldArbitrarySelect -- MonadicFold (to (gridFieldUpdate 0))
                                                                      <|> MonadicFold (to (buttonUpdate "Apply For Funding Now"))
                                                                    ) v
                 ByOrgName targetName -> searchEntities targetName v

               Just _ -> fail "There seems to be some change in the 'Apply for Funding Now' page?"
             case v' ^? isWindowClose of
               Nothing -> return v'
               Just False -> return v'
               Just True -> sendUpdates "Window Close" (MonadicFold (to (buttonUpdate "Continue"))) v'
               
         )

  membersPage <- sendUpdates "Nickname" (    MonadicFold (textFieldArbitrary "Please enter an application nickname here." 255)
               <|> MonadicFold (to (buttonUpdate "Save & Continue"))
              ) v''
    >>= sendUpdates "Contact Info" (    MonadicFold (to (buttonUpdate "Yes"))
                     <|> paragraphArbitraryUpdate "Enter Holiday Contact Information" 4000
                     <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                    )
    >>= sendUpdates "Choose Category" (    MonadicFold (to (buttonUpdate (conf ^. category . to tshow)))
                     <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                    )

  entityInformation <- case membersCompleted membersPage of
    True -> sendUpdates "Entity Members" (MonadicFold (to (buttonUpdate "Save & Continue"))) membersPage
    False -> selectMembers membersPage

  createFRNVal <- sendUpdates "View Entity Types" (MonadicFold (to (buttonUpdate "Save & Continue"))) entityInformation
    >>= sendUpdates "View Discount Rates" (MonadicFold (to (buttonUpdate "Save & Continue")))
  case conf ^. createFRNType of
    CopyFRN _ -> createFRN conf (conf ^. nFRNs) (conf ^. spin) createFRNVal
    NewFRN -> do
      createFRN conf (conf ^. nFRNs) (conf ^. spin) createFRNVal
        >>= forLineItems conf
        -- >>= ifContinueToCertification

    -- Common!
searchEntities :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Text -> Value -> AppianT m Value
searchEntities entityName v = do
  let orgIdents = (^. runFold ((,) <$> Fold (gfColumns . at "Organization ID" . traverse . _TextCellLink . _1) <*> Fold (gfIdentifiers . traverse)) . to (uncurry zip))
  assign appianValue v
  forGridRows1_ sendUpdates orgIdents (MonadicFold $ getGridFieldCell . traverse) (selectEntity entityName)
  sendUpdates1 "Click Apply for Funding Now" (MonadicFold $ to $ buttonUpdate "Apply For Funding Now")
  use appianValue

selectEntity :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Text -> (Text, GridFieldIdent) -> GridField GridFieldCell -> AppianT m ()
selectEntity targetName (name, ident) gf
  | targetName == name = sendUpdates1 ("Select Entity " <> tshow targetName) (MonadicFold $ to (const $ selectCheckbox ident gf) . to toUpdate . to Right)
  | otherwise = return ()

selectMembers :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
selectMembers v = do
  (v', rGridField) <- validMemberCheckboxes v
  case rGridField of
    Error msg -> throwM $ MissingComponentException ("selectMembers: " <> pack msg, v')
    Success gridField -> do
      taskId <- getTaskId v'
      let updates = toUpdate gridField
      checked <- sendUpdate (taskUpdate taskId) $ mkUiUpdate [updates] v'

      sendUpdates "Add Members" (MonadicFold (to (buttonUpdate "Add"))) checked
        >>= sendUpdates "Click Save & Continue" (MonadicFold (to (buttonUpdate "Save & Continue")))

validMemberCheckboxes :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m) => Value -> AppianT m (Value, Result (GridField GridFieldCell))
validMemberCheckboxes v = do
  logDebugN "Getting valid member checkboxes."
  let mRefs = v ^.. getGridFieldRecordRefs "BEN Name" . traverse
  case mRefs of
    [] -> throwM $ BadUpdateException "There are no entity members!" (Just v)
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

viewDiscountRates :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m) => RecordRef -> AppianT m Value
viewDiscountRates rid = do
  v <- viewRecordDashboard rid (Dashboard "summary")
  dashboard <- handleMissing "Discount Rate" v $ v ^? getRecordDashboard "Discount Rate"
  viewRecordDashboard rid dashboard

insufficientDiscountRate :: Value -> Bool
insufficientDiscountRate v = any (checkResult . parseResult) msgs
  where
    msgs = v ^.. hasKeyValue "name" "x-embedded-summary" . key "children" . plate . _String . _JSON . asValue . cosmos . key "validations" . plate . key "message" . _String
    parseResult = parseOnly (manyTill anyChar (string "not sufficient") *> manyTill anyChar (string "Discount Rate") *> pure True)
    checkResult (Left _) = False
    checkResult (Right b) = b

isMultipleEntities :: (Applicative f, Plated s, AsValue s) => (Text -> f Text) -> s -> f s
isMultipleEntities = hasKeyValue "label" "Apply For Funding Now" . key "#t" . _String

isWindowClose :: (Contravariant f, Applicative f, AsValue t) => (Bool -> f Bool) -> t -> f t
isWindowClose = key "title" . _String . to (=="FCC Form 471 Funding Year Window is Closed")

selectFirst :: Applicative f =>
     (GridFieldIdent -> f GridFieldIdent) -> GridField a0 -> f (GridField a0)
selectFirst = gfIdentifiers . traverse . taking 1 traverse

arbTextInt :: MonadGen m => LineItemSize -> m Text
arbTextInt Small = genArbitrary (resize 1 $ QC.arbitrarySizedNatural :: Gen Int) >>= pure . tshow . (+1)
arbTextInt Regular = genArbitrary (QC.arbitrarySizedNatural :: Gen Int) >>= pure . tshow . (+1)

membersCompleted :: Value -> Bool
membersCompleted v = case v ^? deep (filtered $ has $ key "value" . _String . prefixed "We've completed this section of the form based on information from your applicant entity's profile.") of
  Nothing -> False
  Just _ -> True

selectConsortiumMembers :: Value -> Bool
selectConsortiumMembers v = case v ^? getButton "Select an Entity by Consortium Members" of
  Nothing -> False
  Just _ -> True

selectEntities :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
selectEntities v = case selectConsortiumMembers v of
  True -> selectEntitiesReceivingService v
  False -> sendUpdates "Manage Recipients of Service" (MonadicFold (getButtonWith (== "Yes") . to toUpdate . to Right)
                                                       <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                                                      ) v

selectEntitiesReceivingService :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
selectEntitiesReceivingService v
      = sendUpdates "Select Consortia Members" (MonadicFold (to (buttonUpdate "Select an Entity by Consortium Members"))) v
    >>= foldDropdown "Select Consortium Member Entity Type"
                                         (\v -> case v ^? _JSON . to isEmpty470Grid of
                                             Just False -> sendUpdates "Add All Button" addAllButtonUpdate v
                                             _ -> return v
                                         ) "Search for Consortium Member by Entity Type"
    >>= isShared
    -- >>= sendUpdates "Select Consortium Member Entity Type" (MonadicFold (to (dropdownUpdate "Search for Consortium Member by Entity Type" 3)))
    -- >>= sendUpdates "Add All Dependent Schools and NIFs" (MonadicFold (to (buttonUpdate "Add All Dependent Schools And NIFs ")))
    -- >>= sendUpdates "Manage Recipients of Service" (MonadicFold (to (buttonUpdate "Save & Continue")))
    >>= sendUpdates "Manage Recipients of Service" (buttonUpdateWith (\label -> label == "Save & Continue" || label == "Continue") "Could not locate 'Continue' button")

isShared :: (RunClient m, MonadTime m, MonadLogger m, MonadCatch m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
isShared v = case has (getTextField "Are the costs shared equally among all of the entities?") v of
  True -> sendUpdates "Select Yes (Equally Shared Costs)" (MonadicFold (to (buttonUpdate "Yes"))
                                                           <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                                                          ) v
  False -> return v

isEmpty470Grid :: Value -> Bool
isEmpty470Grid v = case v ^? hasType "GridField" . key "numRowsToDisplay" . _Number . only 0.0 of
  Just _ -> True
  Nothing -> False

search470 :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Form470SearchType -> Value -> AppianT m Value
search470 (ByBEN ben) = sendUpdates "Search for 470 by BEN" (MonadicFold (to $ textUpdate "Search by BEN" ben)
                                                      <|> MonadicFold (to (buttonUpdate "Search"))
                                                     )
                        >=> select470
search470 (By470 form470ID) = sendUpdates "Search for 470 by 470 ID" (MonadicFold (to $ textUpdate "Search by FCC Form 470 Number" form470ID)
                                                                      <|> MonadicFold (to $ textUpdate "Search by BEN" mempty)
                                                                      <|> MonadicFold (to (buttonUpdate "Search"))
                                                                     )
                              >=> sendUpdates "Select 470 and Continue" (MonadicFold (to (gridFieldUpdate 0))
                                                                         <|> MonadicFold (to (buttonUpdate "Continue"))
                                                                        )
search470 No470 = sendUpdates "Click Search" (MonadicFold (to $ buttonUpdate "Search"))
                  >=> select470

select470 :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
select470 v = case isEmpty470Grid v of
  True -> sendUpdates "No Form 470" (MonadicFold (to (buttonUpdate "No"))
                                    <|> MonadicFold (to (buttonUpdate "Continue"))
                                    ) v
  False -> sendUpdates "Select 470 and Continue" (MonadicFold (to (gridFieldUpdate 0))
                             <|> MonadicFold (to (buttonUpdate "Continue"))
                            ) v

ifContinueToCertification :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Form471Num
ifContinueToCertification v = do
  formNum <- handleMissing "Could not find 471 number!" v $ v ^? deep (key "title" . _String . to (parseOnly parse471Number) . traverse)
  case has (key "ui" . key "#t" . _String . only "FormLayout") v of
    True -> pure formNum
    False ->
      case v ^? getButton "Review FCC Form 471" of
        Just _ -> sendUpdates "Click Review FCC Form 471" (MonadicFold (to (buttonUpdate "Review FCC Form 471"))) v
          >> pure formNum
        Nothing -> do
          v' <- sendUpdates "Click Continue to Certification" (MonadicFold (to (buttonUpdate "Continue to Certification"))) v
          case v' ^? getButton "Review FCC Form 471" of
            Nothing -> pure formNum
            Just _ -> sendUpdates "Click Review FCC Form 471" (MonadicFold (to (buttonUpdate "Review FCC Form 471"))) v'
              >> pure formNum

    -- Discards the result of f
foldDropdown :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Text -> (Value -> AppianT m Value) -> Text -> Value -> AppianT m Value
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

createFRN :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m, MonadIO m) => Form471Conf -> Int -> Text -> Value -> AppianT m Value
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
                      -- >>= sendUpdates "Continue to Key Information" (MonadicFold $ to $ buttonUpdate "Continue")
                      -- >>= sendUpdates "Continue to Contract" (MonadicFold $ to $ buttonUpdate "Continue")
                      -- >>= sendUpdates "Continue to Contract Dates" (MonadicFold $ to $ buttonUpdate "Continue")
                      -- -- The below may change based on contract type
                      -- >>= setDates
                      -- >>= sendUpdates "Save Narrative & Continue" (MonadicFold (to $ buttonUpdate "Save & Continue"))

  createFRN conf (n - 1) spin frnList

refreshRetryPolicy :: RetryPolicy
refreshRetryPolicy = exponentialBackoff 8000000 `mappend` limitRetries 5

retryRefresh :: MonadIO m => RetryStatus -> Value -> m Bool
retryRefresh _ v = pure $ not $ has (deep $ hasKeyValue "#v" "FRN has been successfully copied.") v

copyFRN :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Form471Conf -> SearchFRNMethod -> Value -> AppianT m Value
copyFRN conf (ByFRNNumber num) = sendUpdates "Search by FRN Number" (MonadicFold (to $ textUpdate "Search by FRN Number" $ tshow num)
                                                                     <|> MonadicFold (to $ buttonUpdate "Search")
                                                                    )
copyFRN conf (ByFCCForm471 num) = sendUpdates "Search by FCC Form 471" (MonadicFold (to $ textUpdate "Search by FCC Form 471" $ tshow num)
                                                                        <|> MonadicFold (to $ buttonUpdate "Search")
                                                                       )

createNewFRN :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Form471Conf -> Text -> Value -> AppianT m Value
createNewFRN conf spin val = do
  dates <- sendUpdates "Funding Request Key Information" (    MonadicFold (textFieldArbitrary "Please enter a Funding Request Nickname here" 255)
                                               <|> MonadicFold (to (buttonUpdate "No"))
                                               <|> MonadicFold (to (dropdownUpdate "Category 1 Service Types" 2))
                                               <|> MonadicFold (to (buttonUpdate "Continue"))
                                              ) val
            >>= sendUpdates "Select Purchase Type" (MonadicFold (to (buttonUpdate "Tariff"))
                              <|> MonadicFold (to (buttonUpdate "Continue"))
                            )
            >>= sendUpdates "Enter number of Bids" ( MonadicFold (intFieldArbitrary "How many bids were received?") -- (to (textUpdate "How many bids were received?" "3"))
                                     <|> MonadicFold (to (buttonUpdate "Yes"))
                                   )
            >>= search470 (conf ^. form470Search)
--             >>= select470
            >>= sendUpdates "SPIN Search" (    MonadicFold (to (textUpdate "Search by SPIN" spin))
                             <|> MonadicFold (to (buttonUpdate "Search"))
                            )
            >>= sendUpdates "Select SPIN and Continue" ( MonadicFold (to (gridFieldUpdate 0))
                            <|> MonadicFold (to (buttonUpdate "Continue"))
                            )

  startDate <- handleMissing "Start Date" dates $ dates ^? hasLabel "What is the service start date?" . dpwValue . appianDate . traverse

  pricing <- sendUpdates "Enter funding dates and Continue" (MonadicFold (to (datePickerUpdate "When will the services end? " (AppianDate $ Just $ addDays 360 startDate)))
              <|> MonadicFold (to (buttonUpdate "Continue"))
              ) dates

  res <- sendUpdates "Price Confidentiality and Continue" (MonadicFold (to (buttonUpdate "No"))
               <|> MonadicFold (to (buttonUpdate "Continue"))
              ) pricing

  narrative <- case res ^? hasKeyValue "label" "Fiber Request Key Information" of
    Nothing -> return res
    Just _ -> sendUpdates "Fiber Request Key Information" (MonadicFold (to $ buttonUpdate "No")
                                                          <|> MonadicFold (to $ buttonUpdate "Continue")
                                                          ) res

  sendUpdates "Enter narrative and Continue" (paragraphArbitraryUpdate "Provide a brief explanation of the products and services that you are requesting, or provide any other relevant information regarding this Funding Request. You should also use this field to describe any updates to your entity data, such as revised student counts, entity relationships, etc, that you were unable to make after the close of the Administrative filing window for profile updates. These changes will be addressed during the application review process." 4000
                           <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                         ) narrative

setDates :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
setDates v = do
  today <- utctDay <$> currentTime
  sendUpdates "Enter funding dates and Continue" (MonadicFold (to (datePickerUpdate "What is the service start date?" (AppianDate $ Just today)))
                                                  <|> MonadicFold (to (datePickerUpdate "What is the date your contract expires for the current term of the contract?" (AppianDate $ Just $ addDays 360 today)))
                                                  <|> MonadicFold (to (buttonUpdate "Continue"))
                                                 ) v

forLineItems :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Form471Conf -> Value -> AppianT m Value
forLineItems conf = forGridRows_ sendUpdates (^. gfColumns . at "FRN" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> addLineItem' conf dyl v)

addLineItem' :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Form471Conf -> DynamicLink -> Value -> AppianT m Value
addLineItem' conf dyl v = sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= addLineItem'' (conf ^. nLineItems)
  >>= sendUpdates "Review FRN Line Items" (buttonUpdateWith (\label -> label == "Continue" || label == "Review FCC Form 471") "Could not locate 'Continue' or 'Review 471 Button'")
  where
    addLineItem'' 0 val = return val
    addLineItem'' n val = do
      logDebugN $ "Creating line item " <> tshow (conf ^. nLineItems - n + 1)
      sendUpdates "Add New FRN Line Item" (MonadicFold (to (buttonUpdate "Add New FRN Line Item"))) val
        >>= selectFunction
        >>= handleDataQuestions
        >>= enterCosts conf
        >>= selectEntities
        >>= sendUpdates "Review Recpients" (MonadicFold (to (buttonUpdate "Continue")))
        >>= (\v -> addLineItem'' (n - 1) v)

selectFunction :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
selectFunction v =
  case has (getDropdown "Function") v of
    True -> sendUpdates "Select Function" (MonadicFold (to (dropdownUpdate "Function" 2))) v
            >>= sendUpdates "Select Type of Connection and Continue" (dropdownArbitraryUpdateF "Type of Connection" -- MonadicFold (to (dropdownUpdate "Type of Connection" 13))
                                                                      -- <|> MonadicFold (radioButtonUpdate "Purpose" 1)
                                                                      <|> MonadicFold (to (buttonUpdate "Continue"))
                                                                     )
    -- False -> sendUpdates "Select Type of Connection" (MonadicFold (to $ dropdownUpdate "Type of Internal Connection" 2)) v
    --   >>= sendUpdates "Select Type, Make, Model, and Continue" (MonadicFold (to $ dropdownUpdate "Type of Product" 2)
    --                                                            <|> dropdownArbitraryUpdateF "Make"
    --     						       <|> MonadicFold (textFieldArbitrary "Enter the Make" 255)
    --                                                            <|> MonadicFold (textFieldArbitrary "Model" 255)
    --                                                            <|> MonadicFold (getButtonWith (== "Yes") . to toUpdate . to Right)
    --                                                            <|> MonadicFold (to $ buttonUpdate "Continue")
    --                                                            )
    False -> sendUpdates "Total Quantity of Equipment Maintained" (MonadicFold (to $ textUpdate "Total Quantity of Equipment Maintained" "2")
                                                                   <|> MonadicFold (to (buttonUpdate "Continue"))
                                                                  ) v

handleDataQuestions :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
handleDataQuestions v = do
  logDebugN "Handling Data Questions"
  case v ^? hasKeyValue "value" "Please enter Bandwidth Speed Information for this Data Transmission and/or Internet Access Line Item" of
    Nothing -> return $ trace "No bandwidth questions" v
    Just _ -> sendUpdates "Not Burstable & Continue" (MonadicFold (to (buttonUpdate "No"))
                                                     <|> MonadicFold (to (buttonUpdate "Continue"))
                                                     ) v
                >>= handleConnections

handleConnections :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
handleConnections v = do
  case has (getDropdown "Connection used by") v of
    True -> sendUpdates "Select All No & Continue" (MonadicFold (getButtonWith (\l -> l == "No" || l == "No ✓") . to toUpdate . to Right)
                                                     <|> MonadicFold (to (dropdownUpdate "Connection used by" 2))
                                                     <|> MonadicFold (to (buttonUpdate "Continue"))
                                                   ) v
    False -> sendUpdates "Select All No & Continue" (MonadicFold (getButtonWith (\l -> l == "No" || l == "No ✓") . to toUpdate . to Right)
                                                     <|> MonadicFold (to (buttonUpdate "Continue"))
                                                   ) v

enterCosts :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Form471Conf -> Value -> AppianT m Value
enterCosts conf v =
  case conf ^. category of
    Cat1 -> case has (hasKeyValue "_cId" "d2d5fc7028c296c76a2a9698ed79bd69") v of
              False -> sendUpdates "Enter Cost Calculations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
                                                                            <|> MonadicFold (act (\v -> textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                            <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                                                                          ) v
              True -> sendUpdates "Enter Cost Calculations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                           <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
                                                                           <|> MonadicFold (act (\v -> textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                           <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                           <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
                                                                           <|> MonadicFold (act (\v -> textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                                           <|> dropdownCidArbitraryUpdateF "d2d5fc7028c296c76a2a9698ed79bd69"
                                                                           <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                                                                         ) v
    Cat2 -> sendUpdates "Enter Costs Calutations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                               <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
                                                               <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt (conf ^. lineItemSize) <*> pure v))
                                                               <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
                                                               <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                                                               ) v

parse471Number :: Parser Form471Num
parse471Number = string "Create FCC Form 471 - " *> (Form471Num <$> decimal)
-- parse471Number = manyTill anyChar (string " - Form # ") *> (Form471Num <$> decimal)

clickThroughAllFRNLineItems :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => Value -> AppianT m Value
clickThroughAllFRNLineItems val = forGridRows_ sendUpdates (^. gfColumns . at "FRN" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> clickThroughFRNLineItems dyl v) val

clickThroughFRNLineItems :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => DynamicLink -> Value -> AppianT m Value
clickThroughFRNLineItems dyl v = sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= forGridRows_ sendUpdates (^. gfColumns . at "FRN Line Item Number" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> clickThroughLineItem dyl v)
  >>= sendUpdates "Continue to FRN Summary" (MonadicFold $ to $ buttonUpdate "Continue")

clickThroughLineItem :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m, MonadGen m, MonadBase IO m, MonadRandom m) => DynamicLink -> Value -> AppianT m Value
clickThroughLineItem dyl v = sendUpdates "Click Line Item" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= sendUpdates "Continue to Cost Calculation" (MonadicFold $ to $ buttonUpdate "Continue")
  >>= sendUpdates "Continue to Recipients of Service" (MonadicFold $ to $ buttonUpdate "Save & Continue")
  >>= sendUpdates "Continue to Line Item Summary" (MonadicFold $ to $ buttonUpdate "Continue")
