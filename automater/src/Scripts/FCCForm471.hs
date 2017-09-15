{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.FCCForm471 where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Attoparsec.Text hiding (Result)
import Data.Time (addDays)

form471Intake :: Appian Value
form471Intake = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport (PathPiece rid)
  form471Link <- handleMissing "FCC Form 471" v' $ v' ^? landingPageLink "FCC Form 471"
  aid <- handleMissing "Action ID" v' $ parseActionId form471Link
  pid <- landingPageAction $ PathPiece aid
  v'' <- ( do
             v <- landingPageActionEx $ PathPiece pid
             case v ^? isWindowClose of
               Nothing -> return v
               Just False -> return v
               Just True -> sendUpdates "Window Close" (Fold (to (buttonUpdate "Continue"))) v
               
         )

  membersPage <- sendUpdates "Nickname" (    Fold (to (textUpdate "Please enter an application nickname here." "PerfTest"))
               <|> Fold (to (buttonUpdate "Save & Continue"))
              ) v''
    >>= sendUpdates "Contact Info" (    Fold (to (buttonUpdate "Yes"))
                     <|> Fold (to (paragraphUpdate "Enter Holiday Contact Information" "Stuff goes in here!"))
                     <|> Fold (to (buttonUpdate "Save & Continue"))
                    )
    >>= sendUpdates "Choose Category" (    Fold (to (buttonUpdate "Category 1"))
                     <|> Fold (to (buttonUpdate "Save & Continue"))
                    )

  entityInformation <- selectMembers membersPage

  dates <- sendUpdates "View Entity Types" (Fold (to (buttonUpdate "Save & Continue"))) entityInformation
            >>= sendUpdates "View Discount Rates" (Fold (to (buttonUpdate "Save & Continue")))
            >>= sendUpdates "Create new FRN" (Fold $ to (buttonUpdate "Add FRN"))
            >>= sendUpdates "Funding Request Key Information" (    Fold (to (textUpdate "Please enter a Funding Request Nickname here" "PerfTest"))
                                               <|> Fold (to (buttonUpdate "No"))
                                               <|> Fold (to (dropdownUpdate "Category 1 Service Types" 2))
                                               <|> Fold (to (buttonUpdate "Continue"))
                                              )
            >>= sendUpdates "Select Purchase Type" (Fold (to (buttonUpdate "Tariff"))
                              <|> Fold (to (buttonUpdate "Continue"))
                            )
            >>= sendUpdates "Enter number of Bids" ( Fold (to (textUpdate "How many bids were received?" "3"))
                                     <|> Fold (to (buttonUpdate "Yes"))
                                   )
            >>= sendUpdates "Search for 470" (Fold $ to (buttonUpdate "Search"))
            >>= sendUpdates "Select 470 and Continue" (Fold (to (gridFieldUpdate 0))
                             <|> Fold (to (buttonUpdate "Continue"))
                            )
            >>= sendUpdates "SPIN Search" (    Fold (to (textUpdate "Search by SPIN" "143000618"))
                             <|> Fold (to (buttonUpdate "Search"))
                            )
            >>= sendUpdates "Select SPIN and Continue" ( Fold (to (gridFieldUpdate 0))
                            <|> Fold (to (buttonUpdate "Continue"))
                            )

  startDate <- handleMissing "Start Date" dates $ dates ^? hasLabel "What is the service start date?" . dpwValue . appianDate . traverse

  pricing <- sendUpdates "Enter funding dates and Continue" (Fold (to (datePickerUpdate "When will the services end? " (AppianDate $ Just $ addDays 360 startDate)))
              <|> Fold (to (buttonUpdate "Continue"))
              ) dates

  narrative <- sendUpdates "Price Confidentiality and Continue" (Fold (to (buttonUpdate "No"))
               <|> Fold (to (buttonUpdate "Continue"))
              ) pricing

  frnList <- sendUpdates "Enter narrative and Continue" (Fold (to (paragraphUpdate "Provide a brief explanation of the products and services that you are requesting, or provide any other relevant information regarding this Funding Request. You should also use this field to describe any updates to your entity data, such as revised student counts, entity relationships, etc, that you were unable to make after the close of the Administrative filing window for profile updates. These changes will be addressed during the application review process." "Some sort of narrative here."))
                           <|> Fold (to (buttonUpdate "Save & Continue"))
                         ) narrative

  sendUpdates "Click FRN Link" (Fold (to (gridFieldDynLinkUpdate "FRN"))) frnList
    >>= sendUpdates "Add New FRN Line Item" (Fold (to (buttonUpdate "Add New FRN Line Item")))
    >>= sendUpdates "Select Function" (Fold (to (dropdownUpdate "Function" 2)))
    >>= sendUpdates "Select Type of Connection and Continue" (Fold (to (dropdownUpdate "Type of Connection" 2))
                    <|> Fold (to (buttonUpdate "Continue"))
                    )
    >>= sendUpdates "Enter Cost Calculations and Continue" (Fold (to $ textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" "1000")
                     <|> Fold (to $ textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" "0")
                     <|> Fold (to $ textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" "3")
                     <|> Fold (to $ textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" "1000")
                     <|> Fold (to $ textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" "0")
                     <|> Fold (to $ textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" "1")
                     <|> Fold (to (buttonUpdate "Save & Continue"))
                    )
    >>= sendUpdates "Select Consortia Members" (Fold (to (buttonUpdate "Select an Entity by Consortium Members")))
    >>= sendUpdates "Select Consortium Member Entity Type" (Fold (to (dropdownUpdate "Search for Consortium Member by Entity Type" 3)))
    >>= sendUpdates "Add All Dependent Schools and NIFs" (Fold (to (buttonUpdate "Add All Dependent Schools And NIFs ")))
    >>= sendUpdates "Continue" (Fold (to (buttonUpdate "Save & Continue")))
    >>= sendUpdates "Review Recpients" (Fold (to (buttonUpdate "Continue")))
    -- End new line item
    >>= sendUpdates "Review FRN Line Items" (Fold (to (buttonUpdate "Continue")))
    >>= sendUpdates "Click Review FCC Form 471" (Fold (to (buttonUpdate "Review FCC Form 471")))

selectMembers :: Value -> Appian Value
selectMembers v = do
  (v', rGridField) <- validMemberCheckboxes v
  case rGridField of
    Error msg -> throwM $ MissingComponentException ("selectMembers: " <> pack msg, v')
    Success gridField -> do
      taskId <- getTaskId v'
      let tid = PathPiece taskId
          updates = toUpdate gridField
      checked <- sendUpdate (taskUpdate tid) $ mkUiUpdate [updates] v'

      sendUpdates "Add Members" (Fold (to (buttonUpdate "Add"))) checked
        >>= sendUpdates "Click Save & Continue" (Fold (to (buttonUpdate "Save & Continue")))

validMemberCheckboxes :: Value -> Appian (Value, Result (GridField GridFieldCell))
validMemberCheckboxes v = do
  liftIO $ putStrLn "Getting valid member checkboxes."
  -- let refs = v ^.. taking 1 (hasType "GridWidget") . getGridWidgetRecordRefs "BEN Name" . to (PathPiece . RecordRef)
  let refs = v ^.. getGridFieldRecordRefs "BEN Name" . traverse . to PathPiece
  discountRates <- mapM (\r -> (,) <$> pure r <*> viewDiscountRates r) refs
  taskId <- getTaskId v
  let validRefs = discountRates ^.. traverse . filtered (has $ _2 . to insufficientDiscountRate . only False) . _1
      tid = PathPiece taskId
      updates = v ^.. to (buttonUpdate "No") . traverse
  v' <- sendUpdate (taskUpdate tid) $ mkUiUpdate updates v
  let gf = v' ^. getGridField
      refs' = gf ^.. traverse . gfColumns . at "BEN Name" . traverse . _TextCellLink . _2 . traverse . to PathPiece
      idents = gf ^.. traverse . gfIdentifiers . traverse . ifolded . ifiltered (\i _ -> i `elem` indices)
      indices = refs ^.. ifolded . filtered (\r -> r `elem` validRefs) . withIndex . _1
  return (v', _Success . gfSelection . gslSelected .~ idents $ gf)
  -- let checkBoxes = v' ^.. taking 1 getGridWidgetValue. gwVal . traverse . filtered (has $ _2 . at "BEN Name" . traverse . deep (key "_recordRef") . _String . to (PathPiece . RecordRef) . filtered (flip elem validRefs)) . _1 . traverse
  -- return (v', checkBoxes)

viewDiscountRates :: PathPiece RecordRef -> Appian Value
viewDiscountRates rid = do
  v <- viewRecordDashboard rid (PathPiece $ Dashboard "summary")
  dashboard <- handleMissing "Discount Rate" v $ v ^? getRecordDashboard "Discount Rate"
  viewRecordDashboard rid $ PathPiece dashboard

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
     (AppianInt -> f AppianInt) -> GridField a0 -> f (GridField a0)
selectFirst = gfIdentifiers . traverse . taking 1 traverse

-- certifyForm471 :: Appian Value
-- certifyForm471
