{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Scripts.Common where

import ClassyPrelude
import Control.Lens hiding (index, Index)
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Control.Lens.Action
import Control.Lens.Action.Reified
import Appian.Internal.Arbitrary
import qualified Data.Foldable as F
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text as A
import qualified Streaming.Prelude as S
import Control.Monad.Logger
import Control.Arrow
import Control.Monad.Time
import Control.Monad.Except
import qualified Data.Csv as Csv
import qualified Data.Attoparsec.Text as T

import Control.Monad.Trans.Resource hiding (throwM)
import Data.Random (MonadRandom)

handleValidations :: MonadThrow m => Either (ScriptError, Value) Value -> AppianT m Value
handleValidations (Right v) = return v
handleValidations (Left (se, _)) = case se ^? _ValidationsError . runFold ((,) <$> Fold _1 <*> Fold _2) of
  Just ((["You must associate at least one Funding Request"], v)) -> return v
  _ -> throwError se

ignoreAssociateValidation :: ScriptError -> Bool
ignoreAssociateValidation (ValidationsError (["You must associate at least one Funding Request"], _, _)) = True
ignoreAssociateValidation _ = False

myLandingPageAction :: RapidFire m => Text -> AppianT m Value
myLandingPageAction actionName = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport rid
  form471Link <- handleMissing actionName v' $ v' ^? landingPageLink actionName
  aid <- handleMissing "Action ID" v' $ parseActionId form471Link
  pid <- landingPageAction aid
  landingPageActionEx pid

myLandingPageAction1 :: RapidFire m => Text -> AppianT m ()
myLandingPageAction1 actionName = myLandingPageAction actionName >>= assign appianValue

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

-- type Updater m = (Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m Value)

    -- Make this use state as soon as the new servant can be used. 
foldGridFieldPagesReport :: RapidFire m => ReportId -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m (b, Value)) -> b -> Value -> AppianT m b
foldGridFieldPagesReport rid = foldGridFieldPages_ (sendReportUpdates rid)

foldGridFieldPages :: RapidFire m => ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m (b, Value)) -> b -> Value -> AppianT m b
foldGridFieldPages = foldGridFieldPages_ sendUpdates

foldGridFieldPages_ :: RapidFire m => Updater m -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> (AppianT m (b, Value))) -> b -> Value -> AppianT m b
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

forGridRows_ :: RapidFire m => Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> Value -> AppianT m Value) -> Value -> AppianT m Value
forGridRows_ = forGridRowsWith_ (const True)

forGridRowsWith_ :: RapidFire m => (Int -> Bool) -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> Value -> AppianT m Value) -> Value -> AppianT m Value
forGridRowsWith_ continueFcn updateFcn colFcn fold f v = do
  gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
  loop (gf ^. gfTotalCount) v 0
    where
      loop total val idx = do
        case (idx < total && continueFcn idx) of
          False -> return val
          True -> do
            (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
            val' <- f b gf val'
            loop total val' (idx + 1)

-- forGridRows1_ :: RapidFire m => Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m ()) -> AppianT m ()
-- forGridRows1_ = forGridRowsWith1_ (const True)

-- forGridRowsWith1_ :: RapidFire m => (Int -> Bool) -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m ()) -> AppianT m ()
-- forGridRowsWith1_ continueFcn updateFcn colFcn fold f = do
--   v <- use appianValue
--   gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
--   loop (gf ^. gfTotalCount) 0
--     where
--       loop total idx = do
--         val <- use appianValue
--         case (idx < total && continueFcn idx) of
--           False -> assign appianValue val
--           True -> do
--             (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
--             res <- deltaUpdate val val'
--             assign appianValue res
--             f b gf
--             loop total (idx + 1)

getArbitraryPagedItems :: (RapidFire m, MonadGen m) => Int -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m ([b], GridField a, Value)
getArbitraryPagedItems nItems updateFcn colFcn fold v = do
  gf <- handleMissing "GridField" v =<< (v ^!? runMonadicFold fold)
  let total = gf ^. gfTotalCount
  indices <- genArbitrary $ take nItems <$> shuffle [0 .. total - 1]
  getArbitraryPagedItems_ indices updateFcn colFcn fold v

getArbitraryPagedItems_ :: RapidFire m => [Int] -> Updater m -> (GridField a -> Vector b) -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m ([b], GridField a, Value)
getArbitraryPagedItems_ indices updateFcn colFcn fold v = F.foldlM getVal Nothing indices >>= handleMissing "Arbitrary items (This needs to be improved)" v
  where
    getVal Nothing idx = do
      (b, gf, val) <- getPagedItem updateFcn colFcn idx fold v
      return $ Just ([b], gf, val)
    getVal (Just (bs, _, val)) idx = do
      (b, gf, val') <- getPagedItem updateFcn colFcn idx fold val
      return $ Just (b : bs, gf, val')

-- newtype StartIndex = StartIndex Int
--   deriving (Show, Eq)

-- newtype RowIndex = RowIndex Int
--   deriving (Show, Eq)

newtype ArbitraryRow = ArbitraryRow (StartIndex, RowIndex)
  deriving (Show, Eq)

instance Arbitrary SortField where
  arbitrary = SortField <$> arbitraryText <*> arbitrary

instance Arbitrary PagingInfo where
  arbitrary = PagingInfo <$> (getPositive <$> arbitrary) <*> arbitrary <*> (getPositive <$> arbitrary)

instance Arbitrary ArbitraryRow where
  arbitrary = ArbitraryRow <$> (getRowIdx <$> (getPositive <$> arbitrary) <*> arbitrary)

-- getPagedItem :: RapidFire m => Updater m -> (GridField a -> Vector b) -> Int -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Value -> AppianT m (b, GridField a, Value)
-- getPagedItem updateFcn colFcn idx fold val = do
--   gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
--   pi <- handleMissing "GridField is not pageable" val $ gf ^? pagingInfo

--   let (startIdx, (RowIndex rowIdx)) = getRowIdx idx pi

--   val' <- getPage updateFcn fold startIdx val
--   gf <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
--   res <- handleMissing ("Row idx: " <> tshow rowIdx) val' $ flip index rowIdx $ colFcn gf
--   return $ (res, gf, val')

arbitraryRow :: Int -> PagingInfo -> Gen ArbitraryRow
arbitraryRow total pi = ArbitraryRow <$> (getRowIdx <$> choose (0, total - 1) <*> pure pi)

-- getRowIdx :: Int -> PagingInfo -> (StartIndex, RowIndex)
-- getRowIdx idx pi = (startIdx, rowIdx)
--   where
--     pageNo = idx `div` pi ^. pgIBatchSize
--     startIdx
--       | pi ^. pgIBatchSize == (-1) = StartIndex 1
--       | otherwise = StartIndex $ pageNo * pi ^. pgIBatchSize + 1
--     rowIdx = RowIndex (idx `rem` pi ^. pgIBatchSize)

data BadPagingException = BadPagingException

instance Show BadPagingException where
  show _ = "It looks like paging is not working correctly!"

instance Exception BadPagingException

--          -- Fetches the page with the given start index. Will throw an
--          -- error if the server responds with a start index that is
--          -- not the same as the given start index.
-- getPage :: RapidFire m => Updater m -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> StartIndex -> Value -> AppianT m Value
-- getPage updateFcn fold idx val = do
--   gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
--   case maybe False (== idx) (gf ^? pagingInfo . pgIStartIndex . to StartIndex) of
--     True -> return val
--     False -> do
--       let gf' = setStartIndex idx gf
--           msg = "Getting page with startIndex: " <> tshow idx

--       val' <- updateFcn msg (MonadicFold $ to $ const $ Right $ toUpdate gf') val
--       gf'' <- handleMissing ("No GridField on the page with startIndex: " <> tshow idx) val' =<< (val' ^!? runMonadicFold fold)
--       case checkPaging idx gf'' of
--         True -> return val'
--         False -> throwM BadPagingException

-- checkPaging :: StartIndex -> GridField a -> Bool
-- checkPaging (StartIndex idx) gf = maybe False (== idx) si
--   where
--     si = gf ^? pagingInfo . pgIStartIndex

-- pagingInfo :: Applicative f => (PagingInfo -> f PagingInfo) -> GridField a -> f (GridField a)
-- pagingInfo = gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable

-- setStartIndex :: StartIndex -> GridField a -> GridField a
-- setStartIndex (StartIndex idx) = pagingInfo . pgIStartIndex .~ idx

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

openReport :: RapidFire m => Text -> AppianT m (ReportId, Value)
openReport reportName = do
  v <- reportsTab
  rid <- getReportId reportName v
  v' <- editReport rid
  return (rid, v')

-- viewRelatedActions :: RapidFire m => Value -> RecordRef -> AppianT m (RecordRef, Value)
-- viewRelatedActions v recordRef = do
--   let ref = recordRef
--   v' <- viewRecordDashboard ref (Dashboard "summary")
--   dashboard <- handleMissing "Related Actions Dashboard" v' $ v' ^? getRecordDashboard "Related Actions"
--   v'' <- viewRecordDashboard ref dashboard
--   return (recordRef, v'')

-- executeRelatedAction :: RapidFire m => Text -> RecordRef -> Value -> AppianT m Value
-- executeRelatedAction action recordId val = do
--   aid <- handleMissing ("could not find actionId for " <> tshow action) val $ val ^? getRelatedActionId action
--   relatedActionEx recordId aid

setAgeSort :: GridField a -> GridField a
setAgeSort = gfSelection . traverse . failing _NonSelectable (_Selectable . gslPagingInfo) . pgISort .~ Just [SortField "secondsSinceRequest" True]

selectGridfieldUpdateF :: GridFieldIdent -> GridField a -> ReifiedMonadicFold m s (Either Text Update)
selectGridfieldUpdateF ident gf = MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))

selectCheckbox :: GridFieldIdent -> GridField a -> GridField a
selectCheckbox ident = gfSelection . traverse . _Selectable . gslSelected .~ [ident]

class Parseable a where
  parseElement :: Text -> Either Text a

-- dropdownUpdate' :: (AsJSON t, AsValue t, Plated t, Parseable s, Eq s, Contravariant f, Applicative f)
--                 => Text -> s -> Over (->) f t t (Either Text Update) (Either Text Update)
-- dropdownUpdate' label s = failing mkUpdate err
--   where
--     fetchSet = getDropdown label . to (dropdownSelect s) . traverse
--     mkUpdate = fetchSet . to toUpdate . to Right
--     err = to $ const $ Left "Could not set dropdown!"

dropdownUpdate' :: (Applicative f, Eq a, Parseable a,
                    Contravariant f)
                => Text -> a -> (Either Text Update -> f (Either Text Update)) -> Value -> f Value
dropdownUpdate' label a = failing (getDropdown label . to Right) (to $ const $ Left $ "Could not find dropdown " <> tshow label) . to (>>= dropdownSelect a) . to (bimap id toUpdate)

dropdownUpdateF' :: (Eq a, Parseable a) =>
                    Text -> a -> ReifiedMonadicFold m Value (Either Text Update)
dropdownUpdateF' label v = MonadicFold $ dropdownUpdate' label v

newtype ParseException = ParseException Text
  deriving Show

instance Exception ParseException

dropdownIndex :: (Parseable s, Eq s) => s -> DropdownField -> Either Text Int
dropdownIndex s df = do
  let filter _ v = v == s

  mRes <- df ^!? dfChoices . itraversed . act parseElement . ifiltered filter . withIndex . _1 . to (+1)
  case mRes of
    Nothing -> Left "Could not find the desired value in the list of choices!"
    Just idx -> Right idx

dropdownSelect :: (Parseable s, Eq s) => s -> DropdownField -> Either Text DropdownField
dropdownSelect s df = do
  idx <- dropdownIndex s df
  return $ dfValue .~ idx $ df

data FundingYear
  = FYSelect
  | FY2016
  | FY2017
  | FY2018
  | FY2019
  | FY2020
  | FYOther Text
  deriving (Show, Eq, Read)

instance Parseable FundingYear where
  parseElement "-- Select a Funding Year --" = pure FYSelect
  parseElement "--Select a Funding Year--" = pure FYSelect
  parseElement "2016" = pure FY2016
  parseElement "2017" = pure FY2017
  parseElement "2018" = pure FY2018
  parseElement "2019" = pure FY2019
  parseElement "2020" = pure FY2020
  parseElement s = pure $ FYOther s -- throwM $ ParseException $ tshow s <> " is not a recognized Funding Year."

-- sendUpdates1 :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
-- sendUpdates1 msg fold = do
--   previousVal <- use appianValue
--   newVal <- sendUpdates msg fold previousVal
--   res <- deltaUpdate previousVal newVal
--   assign appianValue res

-- sendUpdates1' :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m (Either ScriptError ())
-- sendUpdates1' msg fold = do
--   previousVal <- use appianValue
--   eNewVal <- sendUpdates' msg fold previousVal
--   case eNewVal of
--     Left ve -> return $ Left ve
--     Right newVal -> do
--       res <- deltaUpdate previousVal newVal
--       Right <$> assign appianValue res

class HasLogin a where
  getLogin :: a -> Login

instance HasLogin Login where
  getLogin = id

-- | Does not page yet. Hard-coded to only work on the first 50 rows for now too.
arbitraryGridRow :: (RapidFire m, MonadGen m) => (IsSequence row, Index row ~ Int, Element row ~ b) => (GridWidget a -> row) -> (b -> AppianT m c) -> GridWidget a -> AppianT m c
arbitraryGridRow getColumn f gw = do
  x <- genArbitrary $ choose (0, min 49 (gw ^. gwTotalCount - 1))
  let column = getColumn gw
      row = indexEx column x
  f row

newtype ColumnName = ColumnName Text
  deriving (Show, Eq, IsString)

-- | Takes a column name, picks a random row and applies the given function to the randomly selected row.
arbitraryGridRowByColName :: (RapidFire m, MonadGen m) => ColumnName -> (a -> AppianT m b) -> GridWidget a -> AppianT m b
arbitraryGridRowByColName (ColumnName colName) f = arbitraryGridRow (^.. gwVal . traverse . _2 . at colName . traverse) f

arbitraryGridRowByColName1 :: (RapidFire m, MonadGen m) => ColumnName -> (a -> AppianT m b) -> Fold Value (GridWidget a) -> AppianT m b
arbitraryGridRowByColName1 colName f fold = do
    val <- use appianValue
    gw <- handleMissing "Could not find GridWidget" val $ val ^? fold
    arbitraryGridRowByColName colName f gw

instance Parseable Text where
  parseElement = pure

instance Parseable Int where
  parseElement txt = case readMay txt of
    Just n -> Right n
    Nothing -> Left $ "Expected a value of type Int but got " <> tshow txt

newtype DropdownValue = DropdownValue Text
  deriving (Show, Parseable, IsString, Eq)

dropdownUpdateF1 :: Text -> DropdownValue -> ReifiedMonadicFold m Value (Either Text Update)
dropdownUpdateF1 label dv = dropdownUpdateF' label dv

sendUpdates1Handle :: RapidFire m => (ScriptError -> Bool) -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
sendUpdates1Handle handler label fold = do
    eRes <- sendUpdates1' label fold
    case eRes of
        Right _ -> return ()
        Left err -> case handler err of
            False -> throwError err
            True -> return ()

data SelectOrgMethod
  = ByOrgName Text
  | ByArbitrary
  deriving (Show, Ord, Eq)

instance Csv.FromField SelectOrgMethod where
  parseField bs = either failure pure $ parseResult
    where
      parseResult = T.parseOnly parseSearchMethod $ decodeUtf8 bs
      parseSearchMethod = T.string "arbitrary" *> pure ByArbitrary
        <|> T.string "by name: " *> (ByOrgName <$> T.takeText)
      failure = const $ fail $ show bs <> " does not appear to be a valid SelectOrgMethod. Please use only 'arbitrary' or 'by name: <orgName>'"

selectOrganizationOld :: (RapidFire m, MonadGen m) => Text -> ColumnName -> SelectOrgMethod -> Value -> AppianT m Value
selectOrganizationOld buttonName columnName method v = do
  assign appianValue v
  selectOrganization buttonName columnName method
  use appianValue

selectOrganization :: (RapidFire m, MonadGen m) => Text -> ColumnName -> SelectOrgMethod -> AppianT m ()
selectOrganization buttonName _ ByArbitrary = do
  sendUpdates1 "Selecting arbitrary organization" gridFieldArbitrarySelect
  sendUpdates1 ("Click " <> buttonName) (buttonUpdateF "Apply For Funding Now")
selectOrganization buttonName columnName (ByOrgName targetName) = searchEntities buttonName columnName targetName

searchEntities :: (RapidFire m, MonadGen m) => Text -> ColumnName -> Text -> AppianT m ()
searchEntities buttonName (ColumnName columnName) entityName = do
  let orgIdents = (^. runFold ((,) <$> Fold (gfColumns . at columnName . traverse . _TextCellLink . _1) <*> Fold (gfIdentifiers . traverse)) . to (uncurry zip))
  -- assign appianValue v
  forGridRows1_ sendUpdates orgIdents (MonadicFold $ getGridFieldCell . traverse) (selectEntity entityName)
  sendUpdates1 ("Click " <> buttonName) (MonadicFold $ to $ buttonUpdate buttonName)
  -- use appianValue

selectEntity :: (RapidFire m, MonadGen m) => Text -> (Text, GridFieldIdent) -> GridField GridFieldCell -> AppianT m ()
selectEntity targetName (name, ident) gf
  | targetName == name = sendUpdates1 ("Select Entity " <> tshow targetName) (MonadicFold $ to (const $ selectCheckbox ident gf) . to toUpdate . to Right)
  | otherwise = return ()

data MultiOrgStatus
  = IsMultiple
  | IsSingle
  | Failure Text

handleMultipleEntities :: (RapidFire m, MonadGen m) => (Value -> MultiOrgStatus) -> Text -> ColumnName -> SelectOrgMethod -> AppianT m Value
handleMultipleEntities isMultiple buttonName columnName selectMethod = do
  multiple <- usesValue isMultiple
  _ <- case multiple of
         IsSingle -> return ()
         IsMultiple -> selectOrganization buttonName columnName selectMethod
         Failure msg -> scriptError msg

  use appianValue

handleMultipleEntitiesOld :: (RapidFire m, MonadGen m) => (Value -> MultiOrgStatus) -> Text -> ColumnName -> SelectOrgMethod -> Value -> AppianT m Value
handleMultipleEntitiesOld isMultiple buttonName columnName selectMethod v = do
  assign appianValue v
  handleMultipleEntities isMultiple buttonName columnName selectMethod
  use appianValue
  -- case v ^? isMultipleEntities "Apply For Funding Now" of
  --   Nothing -> return v
  --   Just "FormLayout" -> selectOrganizationOld "Apply For Funding Now" selectMethod v
  --   Just _ -> fail "There seems to be some change in the 'Select Organization' page or perhaps it is different for this operation?"
