{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict #-}

module Sheets.Core
  ( RowNum
  , fromRowNum
  , makeRowNum
  , ToSheetRow (..)
  , makeSheet
  , makeWorkbook
  , writeSheet
  , serializeSheet
  , readRows
  , module Codec.Xlsx
  , module Control.Lens
  , makeRow_
  , readVal
  , InvalidSheetRow (..)
  , addViewSheet
  ) where

import ClassyPrelude hiding (throwM)
import ParseCSV
import MachineUtils hiding (RowNum, makeRowNum, fromRowNum)
import Codec.Xlsx
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BL
import Control.Lens hiding (cons)
import Data.Time.Clock.POSIX
import Data.Dynamic

makeRowNum :: ArrowApply cat => ProcessA cat (Event a) (Event (RowNum, a))
makeRowNum = count >>> evMap (\(n, x) -> (RowNum n, x))

newtype RowNum = RowNum Int
  deriving (Show, Eq)

fromRowNum :: RowNum -> Int
fromRowNum (RowNum n) = n

class ToSheetRow a where
  toSheetRow :: RowNum -> a -> Worksheet -> Worksheet
  makeRow :: MonadThrow m => [Text] -> m a
  makeHead :: MonadThrow m => [Text] -> m a

makeRow_ :: (ToSheetRow sheet, ArrowApply a, MonadThrow m) => ProcessA a (Event (RowNum, m [Text])) (Event (RowNum, m sheet))
makeRow_ = dSwitch before after
  where
    makeIt f (rn, mRv) = (rn, join $ fmap f mRv)
    before = proc input -> do
      header <- evMap (makeIt makeHead) -< input
      returnA -< (header, header)
    after _ = proc input -> do
      res <- evMap (makeIt makeRow) -< input
      returnA -< res

makeSheet :: (ArrowApply a, MonadThrow m, ToSheetRow sheet) =>
     ProcessA a (Event (RowNum, m sheet)) (Event (m Worksheet))
makeSheet = proc input -> do
  sheet <- anytime (arr makeSheetRowFcn >>> arr (<*>)) >>> accum (pure def) -< input
  returnA -< sheet <$ input

makeSheetRowFcn :: (Monad m, ToSheetRow sheet) => (RowNum, m sheet) -> m (Worksheet -> Worksheet)
makeSheetRowFcn (rn, mAr) = do
  ar <- mAr
  return $ toSheetRow rn ar

makeWorkbook_ :: (ArrowApply cat, Monad m) => ProcessA cat (Event Text, Event (m Worksheet)) (Event (m Xlsx))
makeWorkbook_ = proc (evtBookName, evtSheet) -> do
  mBookName <- evMap Just >>> hold Nothing -< evtBookName
  mSheet <- evMap Just >>> hold Nothing -< evtSheet
  case mBookName of
    Nothing -> returnA -< noEvent
    Just bookName -> case mSheet of
      Nothing -> returnA -< noEvent
      Just sheet -> do
        wb <- anytime (arr (uncurry makeWorkbookFcn) >>> arr (<*>)) >>> accum (pure def) -< (bookName, sheet) <$ evtSheet
        returnA -< wb <$ evtSheet

makeWorkbook :: (MonadResource m, MonadThrow m1)
  => POSIXTime
  -> ProcessA (Kleisli m) (Event (FilePath)) (Event (m1 Worksheet))
  -> ProcessA (Kleisli m) (Event (Text, FilePath)) (Event (m1 Xlsx))
makeWorkbook t f = evMap fst &&& evMap snd >>> (arr fst &&& switchTest f) >>> makeWorkbook_

makeWorkbookFcn :: Monad m => Text -> m Worksheet -> m (Xlsx -> Xlsx)
makeWorkbookFcn sheetName mWs = do
  ws <- mWs
  return (ws & (atSheet sheetName ?~))

writeSheet :: MonadResource m => FilePath -> ProcessA (Kleisli m) (Event (Either SomeException BL.ByteString)) (Event ())
writeSheet fp = proc input -> do
  mData <- evMap Just >>> hold Nothing -< input
  case mData of
    Nothing -> returnA -< noEvent
    Just eData ->
      case eData of
        Left msg -> machine print -< msg <$ input
        Right dta -> sinkFile fp -< dta <$ input

serializeSheet :: (ArrowApply a, Functor f) =>
     POSIXTime ->
     ProcessA a (Event (f Xlsx)) (Event (f BL.ByteString))
serializeSheet ct = proc input -> do
  mBs <- anytime (arr $ fmap $ fromXlsx ct) >>> evMap Just >>> hold Nothing -< input
  ended <- onEnd -< input
  case mBs of
    Nothing -> returnA -< noEvent
    Just bs -> do
      returnA -< bs <$ ended

readRows :: MonadResource m => CSVSettings -> ProcessA (Kleisli m) (Event Text) (Event (RowNum, Either SomeException [Text]))
readRows settings = machineParser (parseRow settings) >>> makeRowNum

switchTest :: ArrowApply cat => ProcessA cat (Event b) (Event c) -> ProcessA cat (Event a, Event b) (Event c)
switchTest arr = go
  where
    go = proc (evt, input) -> do
      res <- rSwitch arr -< (input, arr <$ evt)
      returnA -< res

data InvalidSheetRow = InvalidSheetRow Text
  deriving Show

instance Exception InvalidSheetRow

readVal :: (MonadThrow m, Read a, Typeable a) => Text -> m a
readVal txtVal = case mVal of
                   Nothing -> throwM $ InvalidSheetRow $ txtVal <> " does not appear to be of type: " <> dispType mVal
                   Just val -> return val
  where
    mVal = readMay txtVal

dispType :: Typeable a => Maybe a -> Text
dispType = pack . intercalate " " . fmap show . typeRepArgs . typeOf

sheetView :: SheetView
sheetView = def & sheetViewPane .~ Just wsPane

wsPane :: Pane
wsPane = Pane (Just PaneTypeBottomLeft) (Just PaneStateFrozen) (Just col) Nothing (Just 1.0)
  where
    col :: CellRef
    col = CellRef "A2"

addViewSheet :: (ArrowApply a, Functor f) =>
     ProcessA a (Event (f Worksheet)) (Event (f Worksheet))
addViewSheet = evMap (fmap (wsSheetViews .~ Just [sheetView]))
