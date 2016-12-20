{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module AggregateSheets where

import ClassyPrelude hiding (throwM)
import Codec.Xlsx
import Control.Lens hiding (cons)
import ParseCSV
import MachineUtils hiding (sampleOn)
import Control.Monad.Trans.Resource
import Data.Dynamic
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (POSIXTime)

data AggregateRow = AggregateRow
  { label :: Text
  , nSamples :: Int
  , average :: Double
  , median :: Double
  , ninetiethPercentLine :: Double
  , ninetyFifthPercentLine :: Double
  , ninetyNinthPercentLine :: Double
  , minVal :: Double
  , maxVal :: Double
  , errorPct :: Double
  , throughput :: Double
  , kbps :: Double
  } deriving (Show, Eq)

newtype RowNum = RowNum Int
  deriving (Show, Eq)

data InvalidAggregateRow = InvalidAggregateRow Text
  deriving Show

instance Exception InvalidAggregateRow

toSheetRow :: RowNum -> AggregateRow -> Worksheet -> Worksheet
toSheetRow (RowNum n) ar ws =
  ws & cellValueAt (n,1) ?~ (CellText $ label ar)
     & cellValueAt (n,2) ?~ (CellDouble $ fromIntegral $ nSamples ar)
     & cellValueAt (n,3) ?~ (CellDouble $ average ar)
     & cellValueAt (n,4) ?~ (CellDouble $ median ar)
     & cellValueAt (n,5) ?~ (CellDouble $ ninetiethPercentLine ar)
     & cellValueAt (n,6) ?~ (CellDouble $ ninetyFifthPercentLine ar)
     & cellValueAt (n,7) ?~ (CellDouble $ ninetyNinthPercentLine ar)
     & cellValueAt (n,8) ?~ (CellDouble $ minVal ar)
     & cellValueAt (n,9) ?~ (CellDouble $ maxVal ar)
     & cellValueAt (n,10) ?~ (CellDouble $ errorPct ar)
     & cellValueAt (n,11) ?~ (CellDouble $ throughput ar)
     & cellValueAt (n,12) ?~ (CellDouble $ kbps ar)
                           
filterFcn :: Either SomeException [Text] -> Bool
filterFcn (Left _) = False
filterFcn (Right (x:_)) = isPrefixOf "AC : " x

readRows :: MonadResource m => ProcessA (Kleisli m) (Event Text) (Event (RowNum, Either SomeException [Text]))
readRows = machineParser (parseRow def) >>> MachineUtils.filter (arr filterFcn) >>> makeRowNum

makeRow :: (ArrowApply a, MonadThrow m) => ProcessA a (Event (RowNum, m [Text])) (Event (RowNum, m AggregateRow))
makeRow = evMap makeIt
  where
    makeIt (rn, mRv) = (rn, join $ fmap makeRow_ mRv)

makeRow_ :: MonadThrow m => [Text] -> m AggregateRow
makeRow_ [l,ns,avg,mdn,ntypct,nfthpct,nnthpct,min,max,error,through,kbps] =
  AggregateRow <$> pure l
               <*> readVal ns
               <*> readVal avg
               <*> readVal mdn
               <*> readVal ntypct
               <*> readVal nfthpct
               <*> readVal nnthpct
               <*> readVal min
               <*> readVal max
               <*> readError error
               <*> readDouble through
               <*> readDouble kbps
  where
    readError = readVal . reverse . drop 1 . reverse
    readDouble v = case isPrefixOf "." v of
      True -> readVal $ '0' `cons` v
      False -> readVal v
makeRow_ row = throwM $ InvalidAggregateRow $ tshow row <> " is not a valid JMeter aggregate report row."

readVal :: (MonadThrow m, Read a, Typeable a) => Text -> m a
readVal txtVal = case mVal of
                   Nothing -> throwM $ InvalidAggregateRow $ txtVal <> " does not appear to be of type: " <> dispType mVal
                   Just val -> return val
  where
    mVal = readMay txtVal

dispType :: Typeable a => Maybe a -> Text
dispType = pack . intercalate " " . fmap show . typeRepArgs . typeOf

count :: ArrowApply cat => ProcessA cat (Event a) (Event (Int, a))
count = proc input -> do
  n <- evMap (const (+1)) >>> accum 0 -< input
  returnA -< fmap (\x -> (n, x)) input

makeRowNum :: ArrowApply cat => ProcessA cat (Event a) (Event (RowNum, a))
makeRowNum = count >>> evMap (\(n, x) -> (RowNum n, x))

makeSheet :: (ArrowApply a, MonadThrow m) =>
     ProcessA a (Event (RowNum, m AggregateRow)) (Event (m Worksheet))
makeSheet = proc input -> do
  sheet <- anytime (arr makeSheetRowFcn >>> arr (<*>)) >>> accum (pure def) -< input
  returnA -< sheet <$ input

makeSheet' :: (Monad m1, MonadThrow m) =>
     ProcessA (Kleisli m1) (Event Text, Event (RowNum, m AggregateRow)) (Event Text, Event (m Worksheet))
makeSheet' = proc (evtSheetName, evtRow) -> do
  mSheetName <- evMap Just >>> dHold Nothing -< evtSheetName
  mRow <- evMap Just >>> hold Nothing -< evtRow
  case mSheetName of
    Nothing -> returnA -< noEvent
    Just sheetName -> do
      sheet <- evMap id *** (anytime (arr makeSheetRowFcn >>> arr (<*>))) >>> tee >>> accum' (pure def) -< (evtSheetName, evtRow)
      returnA -< (evtSheetName, sheet)

makeSheetRowFcn :: Monad m => (RowNum, m AggregateRow) -> m (Worksheet -> Worksheet)
makeSheetRowFcn (rn, mAr) = do
  ar <- mAr
  return $ toSheetRow rn ar

makeWorkbook' :: (Monad m, Monad m1) => ProcessA (Kleisli m1) (Event Text, Event (m Worksheet)) (Event (m Xlsx))
makeWorkbook' = proc (evtBookName, evtSheet) -> do
  mBookName <- evMap Just >>> hold Nothing -< evtBookName
  mSheet <- evMap Just >>> hold Nothing -< evtSheet
  case mBookName of
    Nothing -> returnA -< noEvent
    Just bookName -> case mSheet of
      Nothing -> returnA -< noEvent
      Just sheet -> do
        wb <- anytime (arr (uncurry makeWorkbookFcn) >>> arr (<*>)) >>> accum (pure def) -< (bookName, sheet) <$ evtSheet
        returnA -< wb <$ evtSheet

makeWorkbook :: (ArrowApply cat, MonadThrow m) => ProcessA cat (Event (Text, m Worksheet)) (Event (m Xlsx))
makeWorkbook = proc input -> do
  wb <- anytime (arr (uncurry makeWorkbookFcn) >>> arr (<*>)) >>> accum (return def) -< input
  returnA -< wb <$ input

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

doIt :: MonadResource m =>
  POSIXTime -> 
  ProcessA (Kleisli m) (Event (Text, FilePath)) (Event ())
doIt t = evMap fst &&& evMap snd >>> (arr fst &&& switchTest myArr) >>> makeWorkbook' >>> serializeSheet t >>> writeSheet "/tmp/result.xlsx"
  where
    myArr = sourceFile >>> readRows >>> makeRow >>> makeSheet

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

detectNew :: (ArrowApply cat, Eq a) => ProcessA cat (Event a) (Event b) -> ProcessA cat (Event a) (Event (Either () b))
detectNew arrow = proc input -> do
  mX <- evMap Just >>> hold Nothing -< input
  mXDelayed <- evMap Just >>> dHold Nothing -< input
  case mX of
    Nothing -> returnA -< noEvent
    Just x ->
      case mXDelayed of
        Nothing -> edge >>> arrow >>> evMap Right >>> returnA -< x
        Just xDelayed -> case x == xDelayed of
          True -> edge >>> arrow >>> evMap Right >>> returnA -< x
          False -> do
            res <- edge >>> arrow >>> (evMap (const ()) &&& evMap id) >>> tee -< x
            returnA -< res

detectNew' :: (Monad m, Eq a) => ProcessA (Kleisli m) (Event a) (Event ())
detectNew' = constructT kleisli0 go
  where
    go = do
      v <- await
      yield ()
      loop v
    loop x = do
      v <- await
      case x == v of
        True -> do
          loop v
        False -> do
          yield ()
          loop v

ifNew :: (Monad m, Eq a) => ProcessA (Kleisli m) (Event a) (Event b) -> ProcessA (Kleisli m) (Event a) (Event b)
ifNew arrow = proc input -> do
  mX <- arrow >>> evMap Just >>> hold Nothing -< input
  case mX of
    Nothing -> returnA -< noEvent
    Just x -> do
      isNew <- detectNew' -< input
      returnA -< x <$ isNew

sampleOn :: ArrowApply cat => ProcessA cat (Event a, Event b) (Event a)
sampleOn = proc (input, evt) -> do
  mX <- evMap Just >>> hold Nothing -< input
  case mX of
    Nothing -> returnA -< noEvent
    Just x -> returnA -< x <$ evt

testSwitch :: Monad m => Int -> ProcessA (Kleisli m) (Event (Either a Int)) (Event Int)
testSwitch init = constructT kleisli0 $ loop init
  where
    loop n = do
      mEX <- (Just <$> await) `catchP` pure Nothing
      case mEX of
        Nothing -> yield n
        Just eX -> 
          case eX of
            Left _ -> do
              yield n
              loop 0
            Right x -> do
              loop (n + x)

switchTest :: ArrowApply cat => ProcessA cat (Event b) (Event c) -> ProcessA cat (Event a, Event b) (Event c)
switchTest arr = go
  where
    go = proc (evt, input) -> do
      res <- rSwitch arr -< (input, arr <$ evt)
      returnA -< res

kSwitchTest :: ArrowApply cat => ProcessA cat (Event a, Event b) (Event c) -> ProcessA cat (Event a, Event b) (Event c)
kSwitchTest arr = kSwitch arr trigger switchIt
  where
    trigger = proc ((evt, _), _) -> do
      returnA -< () <$ evt
    switchIt _ _ = arr

checkIt :: ArrowApply cat => ProcessA cat (Event Int) (Event Int)
checkIt = proc input -> do
  mX <- evMap Just >>> hold Nothing -< input
  case mX of
    Nothing -> returnA -< noEvent
    Just x -> case x `rem` 10 of
      0 -> returnA -< input
      n -> returnA -< noEvent

accum' :: Monad m => b -> ProcessA (Kleisli m) (Event (Either a (b -> b))) (Event b)
accum' init = constructT kleisli0 $ loop init
  where
    loop n = do
      mEf <- (Just <$> await) `catchP` pure Nothing
      case mEf of
        Nothing -> yield n
        Just eF ->
          case eF of
            Left _ -> do
              loop init
            Right f -> do
              yield n
              loop (f n)

rangeSource :: Monad m => ProcessA (Kleisli m) (Event Int) (Event Int)
rangeSource = repeatedlyT kleisli0 $ do
  n <- await
  mapM_ yield [0..n]

merge :: ArrowApply cat => ProcessA cat (Event a, Event b) (Event (a, b))
merge = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  mB <- evMap Just >>> hold Nothing -< evtB
  case mA of
    Nothing -> returnA -< noEvent
    Just a -> case mB of
      Nothing -> returnA -< noEvent
      Just b -> returnA -< (a, b) <$ evtB
