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
import Control.Lens
import Control.Lens.Action.Reified
import qualified Data.Foldable as F
import Scripts.Common
import Scripts.Test

assignment :: Appian Value
assignment = do
  (rid, v) <- openReport "Post-Commit Assignments"
  sendReportUpdates rid "Select Review Type" (MonadicFold (to (dropdownUpdate "Review Type" 3))
                                             <|> MonadicFold (to (dropdownUpdate "Reviewer Type" 2))
                                             <|> MonadicFold (to (dropdownUpdate "Funding Year" 2))
                                             <|> MonadicFold (checkboxGroupUpdate "" [1])
                                             <|> MonadicFold (to (buttonUpdate "Apply Filters"))
                                             ) v

openReport :: Text -> Appian (ReportId, Value)
openReport reportName = do
  v <- reportsTab
  rid <- getReportId reportName v
  v' <- editReport (PathPiece rid)
  return (rid, v')
