{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.Time
import Data.Random (MonadRandom)

assignment :: (RunClient m, MonadTime m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBase IO m, MonadRandom m) => ReviewBaseConf -> AppianUsername -> AppianT m Value
assignment conf username = do
  let un = Identifiers [username]
  (rid, v) <- openReport "Post-Commit Assignments"
  sendReportUpdates rid "Select Review Type" (dropdownUpdateF' "Review Type" (reviewType conf)) v
    >>= sendReportUpdates rid "Select Reviewer Type, and Funding Year" (dropdownUpdateF' "Reviewer Type" (reviewerType conf)
                                                                        <|> dropdownUpdateF' "Funding Year" (fundingYear conf)
                                                                       --          <|> MonadicFold (checkboxGroupUpdate "" [1])
                                                                       )
    >>= sendReportUpdates rid "Apply Filters" (MonadicFold (to (buttonUpdate "Apply Filters")))
    >>= sendReportUpdates rid "Sort by Age" (MonadicFold $ getGridFieldCell . traverse . to setAgeSort . to toUpdate . to Right)
    >>= sendReportUpdates rid "Select Case" (MonadicFold $ getGridFieldCell . traverse . to (gridSelection [0..19]) . to toUpdate . to Right)
    >>= sendReportUpdates rid "Select & Assign Reviewer" (MonadicFold (to $ pickerUpdate "Select a Reviewer" un)
                                                 <|> MonadicFold (to (buttonUpdate "Assign Case(s) to Reviewer"))
                                                )
                                                
gridSelection :: [Int] -> GridField a -> GridField a
gridSelection idxs gf = gfSelection . traverse . _Selectable . gslSelected .~ idents $ gf
  where
    idents = gf ^.. gfIdentifiers . traverse . itraversed . withIndex . filtered isDesired . _2
    isDesired (i, _) = i `elem` idxs

