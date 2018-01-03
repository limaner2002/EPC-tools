{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Scripts.ComadReviewTypes where

import ClassyPrelude
import Control.Lens
import Scripts.Common
import qualified Data.Csv as Csv
import Appian.Instances
import Scripts.ReviewCommon (CaseNumber)

data ComadReviewConf = ComadReviewConf
  { _comadId :: CaseNumber
  , _comadReviewer :: Login
  } deriving Show

makeLenses ''ComadReviewConf

instance Csv.FromNamedRecord ComadReviewConf where
  parseNamedRecord r = ComadReviewConf
    <$> r Csv..: "Comad Id"
    <*> Csv.parseNamedRecord r

instance HasLogin ComadReviewConf where
  getLogin conf = conf ^. comadReviewer
