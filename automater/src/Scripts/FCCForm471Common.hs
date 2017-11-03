{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scripts.FCCForm471Common where

import ClassyPrelude
import qualified Data.Csv as Csv

newtype Form471Num = Form471Num Int
  deriving (Show, Eq, Num)

instance Csv.FromField Form471Num where
  parseField bs = Form471Num <$> Csv.parseField bs
