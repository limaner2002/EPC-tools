{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.Assignment where

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

assignment :: ReviewBaseConf -> AppianUsername -> Appian Value
assignment conf username = do
  let un = Identifiers [username]
  (rid, v) <- openReport "Post-Commit Assignments"
  sendReportUpdates rid "Select Review(er) Types, Funding Year, and Apply Filters" (dropdownUpdateF' "Review Type" (reviewType conf)
                                             <|> dropdownUpdateF' "Reviewer Type" (reviewerType conf)
                                             <|> dropdownUpdateF' "Funding Year" (fundingYear conf)
                                             <|> MonadicFold (checkboxGroupUpdate "" [1])
                                             <|> MonadicFold (to (buttonUpdate "Apply Filters"))
                                             ) v
    >>= sendReportUpdates rid "Select Case" (MonadicFold $ getGridFieldCell . traverse . to (gridSelection [0]) . to toUpdate . to Right)
    >>= sendReportUpdates rid "Select & Assign Reviewer" (MonadicFold (to $ pickerUpdate "Select a Reviewer" un)
                                                 <|> MonadicFold (to (buttonUpdate "Assign Case(s) to Reviewer"))
                                                )
                                                
setAgeSort :: GridField a -> GridField a
setAgeSort = gfSelection . traverse . _NonSelectable . pgISort .~ Just [SortField "secondsSinceRequest" True]

gridSelection :: [Int] -> GridField a -> GridField a
gridSelection idxs gf = gfSelection . traverse . _Selectable . gslSelected .~ idents $ gf
  where
    idents = gf ^.. gfIdentifiers . traverse . itraversed . withIndex . filtered isDesired . _2
    isDesired (i, _) = i `elem` idxs

