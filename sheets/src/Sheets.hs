{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sheets where

import ClassyPrelude
import Codec.Xlsx
import Control.Lens
import qualified Control.Foldl as F

class ToCellValue a where
  toCellValue :: a -> CellValue

instance ToCellValue Double where
  toCellValue = CellDouble

instance ToCellValue Text where
  toCellValue = CellText

instance ToCellValue Bool where
  toCellValue = CellBool

instance ToCellValue Int where
  toCellValue = CellDouble . fromIntegral

