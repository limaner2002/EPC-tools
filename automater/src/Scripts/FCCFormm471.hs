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
import Data.Attoparsec.Text
import Data.Time (addDays)

form471Intake :: Appian Value
form471Intake = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport (PathPiece rid)
  form471Link <- handleMissing "FCC Form 471" $ v' ^? landingPageLink "FCC Form 471"
  aid <- handleMissing "Action ID" $ parseActionId form471Link
  pid <- landingPageAction $ PathPiece aid
  v'' <- ( do
             v <- landingPageActionEx $ PathPiece pid
             case v ^? isWindowClose of
               Nothing -> return v
               Just False -> return v
               Just True -> sendUpdates (Fold (to (buttonUpdate "Continue"))) v
               
         )

  membersPage <- sendUpdates (    Fold (to (textUpdate "Please enter an application nickname here." "PerfTest"))
               <|> Fold (to (buttonUpdate "Save & Continue"))
              ) v''
    >>= sendUpdates (    Fold (to (buttonUpdate "Yes"))
                     <|> Fold (to (paragraphUpdate "Enter Holiday Contact Information" "Stuff goes in here!"))
                     <|> Fold (to (buttonUpdate "Save & Continue"))
                    )
    >>= sendUpdates (    Fold (to (buttonUpdate "Category 1"))
                     <|> Fold (to (buttonUpdate "Save & Continue"))
                    )

  entityInformation <- selectMembers membersPage

  listOf470s <- sendUpdates (Fold (to (buttonUpdate "Save & Continue"))) entityInformation
            >>= sendUpdates (Fold (to (buttonUpdate "Save & Continue")))
            >>= sendUpdates (Fold $ to (buttonUpdate "Add FRN"))
            >>= sendUpdates (    Fold (to (textUpdate "Please enter a Funding Request Nickname here" "PerfTest"))
                                               <|> Fold (to (buttonUpdate "No"))
                                               <|> Fold (to (dropdownUpdate "Category 1 Service Types" 2))
                                               <|> Fold (to (buttonUpdate "Continue"))
                                              )
            >>= sendUpdates (Fold (to (buttonUpdate "Tariff"))
                              <|> Fold (to (buttonUpdate "Continue"))
                            )
            >>= sendUpdates ( Fold (to (textUpdate "How many bids were received?" "3"))
                                     <|> Fold (to (buttonUpdate "Yes"))
                                   )
            >>= sendUpdates (Fold $ to (buttonUpdate "Search"))

  checkbox <- handleMissing "Form 470 Grid" $ listOf470s ^? getGridWidgetValue . gwVal . traverse . _1 . traverse

  taskId <- getTaskId listOf470s
  let tid = PathPiece taskId
      updates = listOf470s ^.. runFold (Fold (to (buttonUpdate "Continue") . traverse))
  spinSearch <- sendUpdate (taskUpdate tid) $ mkUiUpdate (toUpdate checkbox : updates) listOf470s

  searchResult <- sendUpdates (    Fold (to (textUpdate "Search by SPIN" "143000661"))
                               <|> Fold (to (buttonUpdate "Search"))
                              ) spinSearch

  return searchResult
  checkbox <- handleMissing "SPIN Grid" $ searchResult ^? getGridWidgetValue . gwVal . traverse . _1 . traverse

  taskId <- getTaskId searchResult
  let tid = PathPiece taskId
      updates = searchResult ^.. runFold (Fold (to (buttonUpdate "Continue") . traverse))
  dates <- sendUpdate (taskUpdate tid) $ mkUiUpdate (toUpdate checkbox : updates) searchResult
  startDate <- handleMissing "Start Date" $ dates ^? hasLabel "What is the service start date?" . dpwValue . appianDate . traverse

  pricing <- sendUpdates (Fold (to (datePickerUpdate "When will the services end? " (AppianDate $ Just $ addDays 360 startDate)))
              <|> Fold (to (buttonUpdate "Continue"))
              ) dates

  narrative <- sendUpdates (Fold (to (buttonUpdate "No"))
               <|> Fold (to (buttonUpdate "Continue"))
              ) pricing

  frnList <- sendUpdates (Fold (to (paragraphUpdate "Provide a brief explanation of the products and services that you are requesting, or provide any other relevant information regarding this Funding Request. You should also use this field to describe any updates to your entity data, such as revised student counts, entity relationships, etc, that you were unable to make after the close of the Administrative filing window for profile updates. These changes will be addressed during the application review process." "Some sort of narrative here."))
                           <|> Fold (to (buttonUpdate "Save & Continue"))
                         ) narrative

  sendUpdates (Fold (to (dynLinksUpdate "FRN" 0))) frnList
    >>= sendUpdates (Fold (to (buttonUpdate "Add New FRN Line Item")))
    >>= sendUpdates (Fold (to (dropdownUpdate "Function" 2)))
    >>= sendUpdates (Fold (to (dropdownUpdate "Type of Connection" 2))
                    <|> Fold (to (buttonUpdate "Continue"))
                    )
    >>= sendUpdates (Fold (to $ textFieldCidUpdate "ee957a1e3a2ca52198084739fbb47ba3" "1000")
                     <|> Fold (to $ textFieldCidUpdate "caeb5787e0d7c381e182e53631fb57ab" "0")
                     <|> Fold (to $ textFieldCidUpdate "962c6b0f58a1bddff3b0d629742c983c" "3")
                     <|> Fold (to $ textFieldCidUpdate "a20962004cc39b76be3d841b402ed5cc" "1000")
                     <|> Fold (to $ textFieldCidUpdate "3664d88f53b3b462acdfebcb53c93b1e" "0")
                     <|> Fold (to $ textFieldCidUpdate "b7c76bf218e1350b13fb987094288670" "1")
                     <|> Fold (to (buttonUpdate "Save & Continue"))
                    )
    >>= sendUpdates (Fold (to (buttonUpdate "Select an Entity by Consortium Members")))
    >>= sendUpdates (Fold (to (dropdownUpdate "Search for Consortium Member by Entity Type" 2)))
    >>= sendUpdates (Fold (getGridWidgetValue . gwVal . traverse . _1 . traverse . to toUpdate . to Right)
                     <|> Fold (to (buttonUpdate "Add"))
                    )
    >>= sendUpdates (Fold (to (buttonUpdate "Save & Continue")))
    >>= sendUpdates (Fold (to (buttonUpdate "Continue")))
    -- End new line item
    >>= sendUpdates (Fold (to (buttonUpdate "Continue")))
    >>= sendUpdates (Fold (to (buttonUpdate "Review FCC Form 471")))

selectMembers :: Value -> Appian Value
selectMembers v = do
  (v', checkboxes) <- validMemberCheckboxes v
  case checkboxes of
    [] -> throwM $ MissingComponentException ("No member checkboxes to select!", v)
    l -> do
      taskId <- getTaskId v'
      let tid = PathPiece taskId
          updates = fmap toUpdate checkboxes
      checked <- sendUpdate (taskUpdate tid) $ mkUiUpdate updates v'

      sendUpdates (Fold (to (buttonUpdate "Add"))) checked
        >>= sendUpdates (Fold (to (buttonUpdate "Save & Continue")))

validMemberCheckboxes :: Value -> Appian (Value, [CheckboxGroup])
validMemberCheckboxes v = do
  liftIO $ putStrLn "Getting valid member checkboxes."
  let refs = v ^.. taking 1 (hasType "GridWidget") . getGridWidgetRecordRefs "BEN Name" . to (PathPiece . RecordRef)
  discountRates <- mapM (\r -> (,) <$> pure r <*> viewDiscountRates r) refs
  taskId <- getTaskId v
  let validRefs = discountRates ^.. traverse . filtered (has $ _2 . to insufficientDiscountRate . only False) . _1
      tid = PathPiece taskId
      updates = v ^.. to (buttonUpdate "No") . traverse
  v' <- sendUpdate (taskUpdate tid) $ mkUiUpdate updates v
  let checkBoxes = v' ^.. taking 1 getGridWidgetValue . gwVal . traverse . filtered (has $ _2 . at "BEN Name" . traverse . deep (key "_recordRef") . _String . to (PathPiece . RecordRef) . filtered (flip elem validRefs)) . _1 . traverse
  return (v', checkBoxes)

viewDiscountRates :: PathPiece RecordRef -> Appian Value
viewDiscountRates rid = do
  v <- viewRecordDashboard rid (PathPiece $ Dashboard "summary")
  dashboard <- handleMissing "Discount Rate" $ v ^? getRecordDashboard "Discount Rate"
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
