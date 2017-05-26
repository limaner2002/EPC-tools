{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Sheets.Generic
  ( Codec.Xlsx.Worksheet
  , Codec.Xlsx.Xlsx
  , Codec.Xlsx.fromXlsx
  , wsColumnFold
  , wsRowFold
  , xlsxSheetFold
  , ToSheetRow
  )where

import ClassyPrelude
import GHC.Generics
import Sheets hiding (ToSheetRow)
import Codec.Xlsx
import qualified Control.Foldl as F
import Control.Lens hiding (from)

class ToSheetRow1 f where
  toSheetRow1 :: f p -> [CellValue]

instance ToCellValue c => ToSheetRow1 (K1 i c) where
  toSheetRow1 (K1 c) = [toCellValue c]

instance ToSheetRow1 f => ToSheetRow1 (M1 i c f) where
  toSheetRow1 (M1 x) = toSheetRow1 x

instance (ToSheetRow1 a, ToSheetRow1 b) => ToSheetRow1 (a :+: b) where
  toSheetRow1 (L1 x) = toSheetRow1 x
  toSheetRow1 (R1 x) = toSheetRow1 x

instance (ToSheetRow1 a, ToSheetRow1 b) => ToSheetRow1 (a :*: b) where
  toSheetRow1 (a :*: b) = toSheetRow1 a <> toSheetRow1 b

defaultToRow :: (Generic a, ToSheetRow1 (Rep a)) => a -> [CellValue]
defaultToRow = toSheetRow1 . from

class ToSheetRow a where
  toSheetRow :: a -> [CellValue]
  default toSheetRow :: (Generic a, ToSheetRow1 (Rep a)) => a -> [CellValue]
  toSheetRow = defaultToRow

wsColumnFold :: Int -> Worksheet -> F.Fold CellValue Worksheet
wsColumnFold n begin = F.Fold stepColumn (0, begin) snd
  where
    stepColumn :: (Int, Worksheet) -> CellValue -> (Int, Worksheet)
    stepColumn (m, ws) val = (m+1, ws & cellValueAt (n,m) ?~ val)

wsRowFold :: ToSheetRow a => F.Fold a Worksheet
wsRowFold = F.Fold stepRow (1, def) snd
  where
    stepRow (n, ws) r = (n+1, F.fold (wsColumnFold n ws) $ toSheetRow r)

xlsxSheetFold :: Xlsx -> F.Fold (Text, Worksheet) Xlsx
xlsxSheetFold begin = F.Fold stepSheet begin id
  where
    stepSheet xlsx (name, ws) = xlsx & atSheet name ?~ ws
