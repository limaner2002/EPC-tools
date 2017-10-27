{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.FCCForm471 where

import ClassyPrelude
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

form471Intake :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Form471Conf -> AppianT m Value
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
               Just "FormLayout" -> sendUpdates "Apply for Funding Now" (gridFieldArbitrarySelect -- MonadicFold (to (gridFieldUpdate 0))
                                                                        <|> MonadicFold (to (buttonUpdate "Apply For Funding Now"))
                                                                        ) v
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
    >>= sendUpdates "Choose Category" (    MonadicFold (to (buttonUpdate "Category 1"))
                     <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                    )

  entityInformation <- case membersCompleted membersPage of
    True -> sendUpdates "Entity Members" (MonadicFold (to (buttonUpdate "Save & Continue"))) membersPage
    False -> selectMembers membersPage

  sendUpdates "View Entity Types" (MonadicFold (to (buttonUpdate "Save & Continue"))) entityInformation
    >>= sendUpdates "View Discount Rates" (MonadicFold (to (buttonUpdate "Save & Continue")))
    >>= createFRN (conf ^. nFRNs) (conf ^. spin)
    >>= forLineItems (conf ^. nLineItems)
    >>= ifContinueToCertification
    >>= sendUpdates "Click Review FCC Form 471" (MonadicFold (to (buttonUpdate "Review FCC Form 471")))

selectMembers :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m Value
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

validMemberCheckboxes :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m (Value, Result (GridField GridFieldCell))
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

viewDiscountRates :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => RecordRef -> AppianT m Value
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

logIt :: MonadIO m => Text -> (a -> ByteString) -> a -> m a
logIt label f x = do
  let fp = "/tmp/" <> unpack label <> ".log"
  writeFile fp $ f x
  return x

arbTextInt :: MonadIO m => m Text
arbTextInt = generate (QC.arbitrarySizedNatural :: Gen Int) >>= pure . tshow . (+1)

membersCompleted :: Value -> Bool
membersCompleted v = case v ^? deep (filtered $ has $ key "value" . _String . prefixed "We've completed this section of the form based on information from your applicant entity's profile.") of
  Nothing -> False
  Just _ -> True

selectConsortiumMembers :: Value -> Bool
selectConsortiumMembers v = case v ^? getButton "Select an Entity by Consortium Members" of
  Nothing -> False
  Just _ -> True

selectEntities :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m Value
selectEntities v = case selectConsortiumMembers v of
  True -> selectEntitiesReceivingService v
  False -> sendUpdates "Manage Recipients of Service" (MonadicFold (to (buttonUpdate "Save & Continue"))) v

selectEntitiesReceivingService :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m Value
selectEntitiesReceivingService v
      = sendUpdates "Select Consortia Members" (MonadicFold (to (buttonUpdate "Select an Entity by Consortium Members"))) v
    >>= foldDropdown "Select Consortium Member Entity Type"
                                         (\v -> case v ^? _JSON . to isEmpty470Grid of
                                             Just False -> sendUpdates "Add All Button" addAllButtonUpdate v
                                             _ -> return v
                                         ) "Search for Consortium Member by Entity Type"
    -- >>= sendUpdates "Select Consortium Member Entity Type" (MonadicFold (to (dropdownUpdate "Search for Consortium Member by Entity Type" 3)))
    -- >>= sendUpdates "Add All Dependent Schools and NIFs" (MonadicFold (to (buttonUpdate "Add All Dependent Schools And NIFs ")))
    >>= sendUpdates "Manage Recipients of Service" (MonadicFold (to (buttonUpdate "Save & Continue")))

isEmpty470Grid :: Value -> Bool
isEmpty470Grid v = case v ^? hasType "GridField" . key "numRowsToDisplay" . _Number . only 0.0 of
  Just _ -> True
  Nothing -> False

select470 :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m Value
select470 v = case isEmpty470Grid v of
  True -> sendUpdates "No Form 470" (MonadicFold (to (buttonUpdate "No"))
                                    <|> MonadicFold (to (buttonUpdate "Continue"))
                                    ) v
  False -> sendUpdates "Select 470 and Continue" (MonadicFold (to (gridFieldUpdate 0))
                             <|> MonadicFold (to (buttonUpdate "Continue"))
                            ) v

ifContinueToCertification :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m Value
ifContinueToCertification v = case v ^? getButton "Review FCC Form 471" of
  Just _ -> return v
  Nothing -> sendUpdates "Click Continue to Certification" (MonadicFold (to (buttonUpdate "Continue to Certification"))) v

    -- Discards the result of f
foldDropdown :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Text -> (Value -> AppianT m Value) -> Text -> Value -> AppianT m Value
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

createFRN :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Int -> Text -> Value -> AppianT m Value
createFRN 0 _ val = return val
createFRN n spin val = do
  logDebugN $ "Creating FRN with " <> tshow (n - 1) <> " to go."
  dates <- sendUpdates "Create new FRN" (MonadicFold $ to (buttonUpdate "Add FRN")) val
            >>= sendUpdates "Funding Request Key Information" (    MonadicFold (textFieldArbitrary "Please enter a Funding Request Nickname here" 255)
                                               <|> MonadicFold (to (buttonUpdate "No"))
                                               <|> MonadicFold (to (dropdownUpdate "Category 1 Service Types" 3))
                                               <|> MonadicFold (to (buttonUpdate "Continue"))
                                              )
            >>= sendUpdates "Select Purchase Type" (MonadicFold (to (buttonUpdate "Tariff"))
                              <|> MonadicFold (to (buttonUpdate "Continue"))
                            )
            >>= sendUpdates "Enter number of Bids" ( MonadicFold (intFieldArbitrary "How many bids were received?") -- (to (textUpdate "How many bids were received?" "3"))
                                     <|> MonadicFold (to (buttonUpdate "Yes"))
                                   )
            >>= sendUpdates "Search for 470" (MonadicFold $ to (buttonUpdate "Search"))
            >>= select470
            -- >>= sendUpdates "Select 470 and Continue" (MonadicFold (to (gridFieldUpdate 0))
            --                  <|> MonadicFold (to (buttonUpdate "Continue"))
            --                 )
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

  frnList <- sendUpdates "Enter narrative and Continue" (paragraphArbitraryUpdate "Provide a brief explanation of the products and services that you are requesting, or provide any other relevant information regarding this Funding Request. You should also use this field to describe any updates to your entity data, such as revised student counts, entity relationships, etc, that you were unable to make after the close of the Administrative filing window for profile updates. These changes will be addressed during the application review process." 4000
                           <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                         ) narrative
  createFRN (n - 1) spin frnList

forLineItems :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Int -> Value -> AppianT m Value
forLineItems nLineItems = forGridRows_ sendUpdates (^. gfColumns . at "FRN" . traverse . _TextCellDynLink . _2) (MonadicFold $ getGridFieldCell . traverse) (\dyl _ v -> addLineItem' nLineItems dyl v)

addLineItem' :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Int -> DynamicLink -> Value -> AppianT m Value
-- addLineItem' 0 dyl val' = return val'
addLineItem' nLineItems dyl v = sendUpdates "Click FRN Link" (MonadicFold (to (const dyl) . to toUpdate . to Right)) v
  >>= addLineItem'' nLineItems
  >>= sendUpdates "Review FRN Line Items" (MonadicFold (to (buttonUpdate "Continue")))
  where
    addLineItem'' 0 val = return val
    addLineItem'' n val = do
      logDebugN $ "Creating line item " <> tshow (nLineItems - n + 1)
      sendUpdates "Add New FRN Line Item" (MonadicFold (to (buttonUpdate "Add New FRN Line Item"))) val
        >>= sendUpdates "Select Function" (dropdownArbitraryUpdateF "Function")
        >>= sendUpdates "Select Type of Connection and Continue" (dropdownArbitraryUpdateF "Type of Connection"
                                                                  <|> radioArbitraryF "Purpose"
                                                                  <|> MonadicFold (to (buttonUpdate "Continue"))
                                                                 )
        >>= handleDataQuestions
        >>= sendUpdates "Enter Cost Calculations and Continue" (MonadicFold (act (\v -> textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" <$> arbTextInt <*> pure v))
                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" <$> pure "0" <*> pure v))
                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" <$> arbTextInt <*> pure v))
                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" <$> arbTextInt <*> pure v))
                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" <$> pure "0" <*> pure v))
                                                                <|> MonadicFold (act (\v -> textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" <$> arbTextInt <*> pure v))
                                                                <|> MonadicFold (to (buttonUpdate "Save & Continue"))
                                                               )
        >>= selectEntities
        >>= sendUpdates "Review Recpients" (MonadicFold (to (buttonUpdate "Continue")))
        >>= (\v -> addLineItem'' (n - 1) v)

handleDataQuestions :: (MonadCatch m, MonadLogger m, MonadIO m, RunClient m) => Value -> AppianT m Value
handleDataQuestions v = do
  logDebugN "Handling Data Questions"
  case v ^? hasKeyValue "value" "Please enter Bandwidth Speed Information for this Data Transmission and/or Internet Access Line Item" of
    Nothing -> return $ trace "No bandwidth questions" v
    Just _ -> sendUpdates "Not Burstable & Continue" (MonadicFold (to (buttonUpdate "No"))
                                                     <|> MonadicFold (to (buttonUpdate "Continue"))
                                                     ) v
                >>= sendUpdates "Select All No & Continue" (MonadicFold (getButtonWith (\l -> l == "No" || l == "No ✓") . to toUpdate . to Right)
                                                           <|> MonadicFold (to (buttonUpdate "Continue"))
                                                           )

data Form471Conf = Form471Conf
  { _nFRNs :: Int
  , _nLineItems :: Int
  , _spin :: Text
  , _applicant :: Login
  } deriving Show

instance Csv.FromNamedRecord Form471Conf where
  parseNamedRecord r = Form471Conf
    <$> r Csv..: "nFRNs"
    <*> r Csv..: "nLineItems"
    <*> r Csv..: "spin"
    <*> Csv.parseNamedRecord r

nFRNs :: Functor f => (Int -> f Int) -> Form471Conf -> f Form471Conf
nFRNs = lens get update
  where
    get = _nFRNs
    update conf v = conf { _nFRNs = v }

nLineItems :: Functor f => (Int -> f Int) -> Form471Conf -> f Form471Conf
nLineItems = lens get update
  where
    get = _nLineItems
    update conf v = conf { _nLineItems = v }

spin :: Functor f => (Text -> f Text) -> Form471Conf -> f Form471Conf
spin = lens get update
  where
    get = _spin
    update conf v = conf { _spin = v }

applicant :: Functor f => (Login -> f Login) -> Form471Conf -> f Form471Conf
applicant = lens get update
  where
    get = _applicant
    update conf v = conf { _applicant = v }
