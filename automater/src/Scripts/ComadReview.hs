{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.ComadReview where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action.Reified
import Scripts.Common
import Scripts.ReviewCommon
import Scripts.Test

-- comadReview :: Appian Value
-- comadReview = 
  
editAdjustmentAmounts :: RecordRef -> Appian Value
editAdjustmentAmounts rid =
  viewRecordDashboard (PathPiece rid) (PathPiece $ Dashboard "summary")
    >>= flip viewRelatedActions rid
    >>= uncurry (executeRelatedAction "Review COMAD Request")
    >>= pageAdjustments
    >>= sendUpdates "Submit" (MonadicFold $ to $ buttonUpdate "Submit")

pageAdjustments :: Value -> Appian Value
pageAdjustments val = foldGridFieldPages (MonadicFold $ getGridFieldCell . traverse) editAdjustments val val

editAdjustments :: Value -> GridField GridFieldCell -> Appian (Value, Value)
editAdjustments val gf = do
  v <- foldGridField' editAdjustment val gf
  return (v, v)

selectGridfieldUpdateF :: AppianInt -> GridField a -> ReifiedMonadicFold m s (Either Text Update)
selectGridfieldUpdateF ident gf = MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))

editAdjustment :: Value -> AppianInt -> Appian Value
editAdjustment val ident = do
  gf <- handleMissing "FRN Adjustment Grid" val $ val ^? getGridFieldCell . traverse
  val' <- sendUpdates "Adjustments: Select FRN Checkbox" (selectGridfieldUpdateF ident gf) val
  sendUpdates "Continue to Edit FRN Adjustment" (MonadicFold $ to $ buttonUpdate "Continue") val'
    >>= sendUpdates "Enter FRN Data" (intFieldArbitraryUpdateF "COMAD Overlap Amount"
                                     <|> intFieldArbitraryUpdateF "COMAD Recover Overlap"
                                     <|> intFieldArbitraryUpdateF "RIDF Overlap"
                                     <|> intFieldArbitraryUpdateF "COMAD/RIDF Overlap"
                                     <|> intFieldArbitraryUpdateF "FRN Cross-COMAD Request Overlap"
                                     <|> MonadicFold (to $ buttonUpdate "Calculate")
                                     <|> MonadicFold (to $ buttonUpdate "Save")
                                     )
