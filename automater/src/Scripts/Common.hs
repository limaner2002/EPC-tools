{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.Common where

import ClassyPrelude
import Control.Lens hiding (index)
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Control.Lens.Action
import Control.Lens.Action.Reified
import Scripts.Test
import qualified Data.Foldable as F
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text as A
import qualified Streaming.Prelude as S
import Control.Monad.Logger
import Control.Arrow
import Control.Monad.Time

import Control.Monad.Trans.Resource hiding (throwM)

handleValidations :: MonadThrow m => Either ValidationsException Value -> AppianT m Value
handleValidations (Right v) = return v
handleValidations (Left ve) = case ve ^. validationsExc . _1 of
  ["You must associate at least one Funding Request"] -> do
    return $ ve ^. validationsExc . _2
  _ -> throwM ve

myLandingPageAction :: (MonadThrow m, RunClient m) => Text -> AppianT m Value
myLandingPageAction actionName = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport rid
  form471Link <- handleMissing actionName v' $ v' ^? landingPageLink actionName
  aid <- handleMissing "Action ID" v' $ parseActionId form471Link
  pid <- landingPageAction aid
  landingPageActionEx pid

addAllFRNsButtonUpdate :: (Plated s, AsValue s, AsJSON s) => ReifiedMonadicFold m s (Either Text Update)
addAllFRNsButtonUpdate = MonadicFold (failing (getButtonWith addAllFRNsButton . to toUpdate . to Right) (to (const $ Left "Could not find the Add All FRNs button!")))

addAllFRNsButton :: Text -> Bool
addAllFRNsButton label = isPrefixOf "Add all " label && isSuffixOf " FRNs" label

foldGridField :: Monad m => (b -> a -> AppianT m b) -> Text -> b -> GridField a -> AppianT m b
foldGridField f column b gf = do
  let col = gf ^.. gfColumns . at column . traverse
  F.foldlM f b col

foldGridField' :: Monad m => (b -> GridFieldIdent -> AppianT m b) -> b -> GridField a -> AppianT m b
foldGridField' f b gf = do
  let boxes = gf ^.. gfIdentifiers . traverse . traverse
  F.foldlM f b boxes

type Updater m = (Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m Value)

    -- Make this use state as soon as the new servant can be used. 
foldGridFieldPagesReport :: (MonadLogger m, RunClient m, MonadTime m, MonadCatch m) => ReportId -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m (b, Value)) -> b -> Value -> AppianT m b
foldGridFieldPagesReport rid = foldGridFieldPages_ (sendReportUpdates rid)

foldGridFieldPages :: (MonadLogger m, RunClient m, MonadTime m, MonadCatch m) => ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m (b, Value)) -> b -> Value -> AppianT m b
foldGridFieldPages = foldGridFieldPages_ sendUpdates

foldGridFieldPages_ :: (MonadLogger m, RunClient m, MonadThrow m) => Updater m -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> (AppianT m (b, Value))) -> b -> Value -> AppianT m b
foldGridFieldPages_ updateFcn fold f b v = loop b v
  where
    loop accum val = do
      gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
      logDebugN $ "PagingInfo: " <> (tshow $ gf ^? pagingInfo)
      case nextPage gf gf of
        Nothing -> do
          (accum, _) <- f accum gf
          return accum
        Just _ -> do
          (accum', val') <- f accum gf
          gf' <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
          logDebugN $ "PagingInfo: " <> (tshow $ gf' ^? pagingInfo)
          loop accum' =<< getNextPage_ updateFcn gf gf' val'

forGridRows_ :: (RunClient m, MonadThrow m) => Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> Value -> AppianT m Value) -> Value -> AppianT m Value
forGridRows_ updateFcn colFcn fold f v = do
  gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
  loop (gf ^. gfTotalCount) v 0
    where
      loop total val idx = do
        case total <= idx of
          True -> return val
          False -> do
            (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
            val' <- f b gf val'
            loop total val' (idx + 1)

forGridRows1_ :: (RunClient m, MonadThrow m) => Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m ()) -> AppianT m ()
forGridRows1_ updateFcn colFcn fold f = do
  v <- use appianValue
  gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
  loop (gf ^. gfTotalCount) 0
    where
      loop total idx = do
        val <- use appianValue
        case total <= idx of
          True -> assign appianValue val
          False -> do
            (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
            res <- deltaUpdate val val'
            assign appianValue res
            f b gf
            loop total (idx + 1)

getArbitraryPagedItems :: (RunClient m, MonadThrow m, MonadGen m) => Int -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m ([b], GridField a, Value)
getArbitraryPagedItems nItems updateFcn colFcn fold v = do
  gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
  let total = gf ^. gfTotalCount
  indices <- genArbitrary $ take nItems <$> shuffle [0 .. total - 1]
  getArbitraryPagedItems_ indices updateFcn colFcn fold v

getArbitraryPagedItems_ :: (RunClient m, MonadThrow m) => [Int] -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m ([b], GridField a, Value)
getArbitraryPagedItems_ indices updateFcn colFcn fold v = F.foldlM getVal Nothing indices >>= handleMissing "Arbitrary items (This needs to be improved)" v
  where
    getVal Nothing idx = do
      (b, gf, val) <- getPagedItem updateFcn colFcn idx fold v
      return $ Just ([b], gf, val)
    getVal (Just (bs, _, val)) idx = do
      (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
      return $ Just (b : bs, gf, val')

newtype StartIndex = StartIndex Int
  deriving (Show, Eq)

newtype RowIndex = RowIndex Int
  deriving (Show, Eq)

newtype ArbitraryRow = ArbitraryRow (StartIndex, RowIndex)
  deriving (Show, Eq)

instance Arbitrary SortField where
  arbitrary = SortField <$> arbitraryText <*> arbitrary

instance Arbitrary PagingInfo where
  arbitrary = PagingInfo <$> (getPositive <$> arbitrary) <*> arbitrary <*> (getPositive <$> arbitrary)

instance Arbitrary ArbitraryRow where
  arbitrary = ArbitraryRow <$> (getRowIdx <$> (getPositive <$> arbitrary) <*> arbitrary)

getPagedItem :: (RunClient m, MonadThrow m) => Updater m -> (GridField a -> Vector b) -> Int -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m (b, GridField a, Value)
getPagedItem updateFcn colFcn idx fold val = do
  gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
  pi <- handleMissing "GridField is not pageable" val $ gf ^? pagingInfo

  let (startIdx, (RowIndex rowIdx)) = getRowIdx idx pi

  val' <- getPage updateFcn fold startIdx val
  gf <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
  res <- handleMissing ("Row idx: " <> tshow rowIdx) val' $ flip index rowIdx $ colFcn gf
  return $ (res, gf, val')

arbitraryRow :: Int -> PagingInfo -> Gen ArbitraryRow
arbitraryRow total pi = ArbitraryRow <$> (getRowIdx <$> choose (0, total - 1) <*> pure pi)

getRowIdx :: Int -> PagingInfo -> (StartIndex, RowIndex)
getRowIdx idx pi = (startIdx, rowIdx)
  where
    pageNo = idx `div` pi ^. pgIBatchSize
    startIdx
      | pi ^. pgIBatchSize == (-1) = StartIndex 1
      | otherwise = StartIndex $ pageNo * pi ^. pgIBatchSize + 1
    rowIdx = RowIndex (idx `rem` pi ^. pgIBatchSize)

data BadPagingException = BadPagingException

instance Show BadPagingException where
  show _ = "It looks like paging is not working correctly!"

instance Exception BadPagingException

         -- Fetches the page with the given start index. Will throw an
         -- error if the server responds with a start index that is
         -- not the same as the given start index.
getPage :: (RunClient m, MonadThrow m) => Updater m -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> StartIndex -> Value -> AppianT m Value
getPage updateFcn fold idx val = do
  gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
  case maybe False (== idx) (gf ^? pagingInfo . pgIStartIndex . to StartIndex) of
    True -> return val
    False -> do
      let gf' = setStartIndex idx gf
          msg = "Getting page with startIndex: " <> tshow idx

      val' <- updateFcn msg (MonadicFold $ to $ const $ Right $ toUpdate gf') val
      gf'' <- handleMissing ("No GridField on the page with startIndex: " <> tshow idx) val' =<< (val' ^!? runMonadicFold fold)
      case checkPaging idx gf'' of
        True -> return val'
        False -> throwM BadPagingException

checkPaging :: StartIndex -> GridField a -> Bool
checkPaging (StartIndex idx) gf = maybe False (== idx) si
  where
    si = gf ^? pagingInfo . pgIStartIndex

pagingInfo :: Applicative f => (PagingInfo -> f PagingInfo) -> GridField a -> f (GridField a)
pagingInfo = gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable

setStartIndex :: StartIndex -> GridField a -> GridField a
setStartIndex (StartIndex idx) = pagingInfo . pgIStartIndex .~ idx

getNextPage_ :: RunClient m => Updater m
             -> GridField a -> GridField a -> Value -> AppianT m Value
getNextPage_ updateFcn gf gf' = updateFcn "Next Page" (MonadicFold $ to $ const gf'')
  where
    gf'' = toUpdate <$> (maybeToEither "This is the last page!" $ nextPage gf gf')

nextPage :: GridField a -> GridField a -> Maybe (GridField a)
nextPage gf gf' = do
  pi <- gf ^? pagingInfo
  let pi' = pgIStartIndex %~ (+batchSize) $ pi
      batchSize = pi ^. pgIBatchSize
  case pi' ^. pgIStartIndex > gf ^. gfTotalCount of
    True -> Nothing
    False -> return $ pagingInfo .~ pi' $ gf'

getNumber :: Parser Text
getNumber = takeTill (== '#') *> A.take 1 *> takeTill (== ' ')

parseNumber :: Text -> Either String Text
parseNumber = parseOnly getNumber

data DistributeTask
  = Produce
  | Consume
  deriving Show

distributeTasks :: MonadIO m => TVar DistributeTask -> TChan (ThreadControl a) -> S.Stream (S.Of (ThreadControl a)) m () -> (a -> m b) -> m (Maybe b)
distributeTasks taskVar chan producer f = do
  task <- g
  case task of
    Produce -> do
      S.mapM_ (atomically . writeTChan chan) $ producer
      atomically $ writeTChan chan Finished
      consumer
    Consume -> consumer
  where
    g = atomically $ do
      task <- readTVar taskVar
      case task of
        Produce -> do
          writeTVar taskVar Consume
          return task
        _ -> return Consume
    consumer = do
      tc <- atomically $ readTChan chan
      case tc of
        Finished -> return Nothing
        Item a -> f a >>= pure . Just

data ThreadControl a
  = Item a
  | Finished

notFinished Finished = False
notFinished _ = True

tcItem (Item a) = a
tcItem _ = error "This should have already terminated!"

openReport :: (RunClient m, MonadThrow m) => Text -> AppianT m (ReportId, Value)
openReport reportName = do
  v <- reportsTab
  rid <- getReportId reportName v
  v' <- editReport rid
  return (rid, v')

viewRelatedActions :: (RunClient m, MonadThrow m) => Value -> RecordRef -> AppianT m (RecordRef, Value)
viewRelatedActions v recordRef = do
  let ref = recordRef
  v' <- viewRecordDashboard ref (Dashboard "summary")
  dashboard <- handleMissing "Related Actions Dashboard" v' $ v' ^? getRecordDashboard "Related Actions"
  v'' <- viewRecordDashboard ref dashboard
  return (recordRef, v'')

executeRelatedAction :: (RunClient m, MonadThrow m) => Text -> RecordRef -> Value -> AppianT m Value
executeRelatedAction action recordId val = do
  aid <- handleMissing ("could not find actionId for " <> tshow action) val $ val ^? getRelatedActionId action
  relatedActionEx recordId aid

setAgeSort :: GridField a -> GridField a
setAgeSort = gfSelection . traverse . failing _NonSelectable (_Selectable . gslPagingInfo) . pgISort .~ Just [SortField "secondsSinceRequest" True]

selectGridfieldUpdateF :: GridFieldIdent -> GridField a -> ReifiedMonadicFold m s (Either Text Update)
selectGridfieldUpdateF ident gf = MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))

selectCheckbox :: GridFieldIdent -> GridField a -> GridField a
selectCheckbox ident = gfSelection . traverse . _Selectable . gslSelected .~ [ident]

class Parseable a where
  parseElement :: MonadThrow m => Text -> m a

-- dropdownUpdate' :: (AsJSON t, AsValue t, Plated t, Parseable s, Eq s, Contravariant f, Applicative f)
--                 => Text -> s -> Over (->) f t t (Either Text Update) (Either Text Update)
-- dropdownUpdate' label s = failing mkUpdate err
--   where
--     fetchSet = getDropdown label . to (dropdownSelect s) . traverse
--     mkUpdate = fetchSet . to toUpdate . to Right
--     err = to $ const $ Left "Could not set dropdown!"

dropdownUpdate' :: (Applicative f, Eq a, Parseable a,
                    Contravariant f, Plated b, Data.Aeson.Lens.AsValue b, AsJSON b
                   ) => Text -> a -> (Either Text Update -> f (Either Text Update)) -> b -> f b
dropdownUpdate' label a = getDropdown label . to (dropdownSelect a) . to (bimap tshow toUpdate)

dropdownUpdateF' :: (Eq a, Parseable a, Plated s, AsValue s, AsJSON s) =>
                    Text -> a -> ReifiedMonadicFold m s (Either Text Update)
dropdownUpdateF' label s = MonadicFold $ dropdownUpdate' label s

newtype ParseException = ParseException Text
  deriving Show

instance Exception ParseException

dropdownIndex :: (Parseable s, Eq s, MonadThrow m) => s -> DropdownField -> m Int
dropdownIndex s df = do
  let filter _ v = v == s

  mRes <- df ^!? dfChoices . itraversed . act parseElement . ifiltered filter . withIndex . _1 . to (+1)
  case mRes of
    Nothing -> throwM $ ParseException $ "Could not find the desired value in the list of choices!"
    Just idx -> return idx

dropdownSelect :: (Parseable s, Eq s, MonadThrow m) => s -> DropdownField -> m DropdownField
dropdownSelect s df = do
  idx <- dropdownIndex s df
  return $ dfValue .~ idx $ df

data FundingYear
  = FYSelect
  | FY2016
  | FY2017
  | FY2018
  deriving (Show, Eq, Read)

instance Parseable FundingYear where
  parseElement "-- Select a Funding Year --" = pure FYSelect
  parseElement "2016" = pure FY2016
  parseElement "2017" = pure FY2017
  parseElement s = throwM $ ParseException $ tshow s <> " is not a recognized Funding Year."

sendUpdates1 :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m) => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
sendUpdates1 msg fold = do
  previousVal <- use appianValue
  newVal <- sendUpdates msg fold previousVal
  res <- deltaUpdate previousVal newVal
  assign appianValue res

sendUpdates1' :: (MonadCatch m, MonadLogger m, MonadTime m, RunClient m) => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m (Either ValidationsException ())
sendUpdates1' msg fold = do
  previousVal <- use appianValue
  eNewVal <- sendUpdates' msg fold previousVal
  case eNewVal of
    Left ve -> return $ Left ve
    Right newVal -> do
      res <- deltaUpdate previousVal newVal
      Right <$> assign appianValue res

deltaUpdate :: (Monad m, MonadThrow m) => Value -> Value -> AppianT m Value
deltaUpdate full delta =
    case has (key "ui" . key "#t" . _String . only "UiComponentsDelta") delta of
      False -> return $ trace ("type is " <> delta ^. key "ui" . key "#t" . _String . to unpack)  delta
      True -> handleMissing "Bad update delta?" full $ handleDelta full $ trace ("type is " <> delta ^. key "ui" . key "#t" . _String . to unpack) delta

handleDelta :: Value -> Value -> Maybe Value
handleDelta fullResp delta = do
  let comps = delta ^.. key "ui" . key "modifiedComponents" . plate
  foldlM updateComponent fullResp comps

updateComponent :: Value -> Value -> Maybe Value
updateComponent fullResp componentVal = do
  cid <- componentVal ^? key "_cId" . _String
  return $ (hasKeyValue "_cId" cid .~ componentVal) fullResp

class HasLogin a where
  getLogin :: a -> Login
