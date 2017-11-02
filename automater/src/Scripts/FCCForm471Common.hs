{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scripts.FCCForm471Common where

import ClassyPrelude

newtype Form471Num = Form471Num Int
  deriving (Show, Eq, Num)
