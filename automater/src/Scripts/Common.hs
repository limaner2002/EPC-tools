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
import Data.Attoparsec.Text
import qualified Streaming.Prelude as S
import Control.Monad.Logger
import Control.Arrow

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

type Updater m = (Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> AppianT m Value)

    -- Make this use state as soon as the new servant can be used. 
foldGridFieldPagesReport :: (MonadLogger m, RunClient m, MonadIO m, MonadCatch m) => ReportId -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m (b, Value)) -> b -> Value -> AppianT m b
foldGridFieldPagesReport rid = foldGridFieldPages_ (sendReportUpdates rid)

foldGridFieldPages :: (MonadLogger m, RunClient m, MonadIO m, MonadCatch m) => ReifiedMonadicFold (AppianT m) Value (GridField a) -> (b -> GridField a -> AppianT m (b, Value)) -> b -> Value -> AppianT m b
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
forGridRows_ updateFcn colFcn fold f v = loop v 0
  where
    loop val idx = do
      gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
      let mPagingInfo = gf ^? pagingInfo
      case mPagingInfo of
        Nothing -> return val
        Just pi -> do
          case gf ^. gfTotalCount <= idx of
            True -> return val
            False -> do
              let pageNo = idx `div` pi ^. pgIBatchSize
                  startIdx = pageNo * pi ^. pgIBatchSize + 1
                  rowIdx = idx `rem` pi ^. pgIBatchSize
              val' <- getPage updateFcn fold startIdx val
              gf' <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
              b <- handleMissing ("Row idx: " <> tshow rowIdx) val' $ flip index rowIdx $ colFcn gf'
              val'' <- f b gf' val'
              loop val'' (idx + 1)

data BadPagingException = BadPagingException

instance Show BadPagingException where
  show _ = "It looks like paging is not working correctly!"

instance Exception BadPagingException

         -- Fetches the page with the given start index. Will throw an
         -- error if the server responds with a start index that is
         -- not the same as the given start index.
getPage :: (RunClient m, MonadThrow m) => Updater m -> ReifiedMonadicFold (AppianT m) Value (GridField a) -> Int -> Value -> AppianT m Value
getPage updateFcn fold idx val = do
  gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
  case maybe False (== idx) (gf ^? pagingInfo . pgIStartIndex) of
    True -> return val
    False -> do
      let gf' = setStartIndex idx gf
          msg = "Getting page with startIndex: " <> tshow idx

      val' <- updateFcn msg (MonadicFold $ to $ const $ Right $ toUpdate gf') val
      gf'' <- handleMissing ("No GridField on the page with startIndex: " <> tshow idx) val' =<< (val' ^!? runMonadicFold fold)
      case checkPaging idx gf'' of
        True -> return val'
        False -> throwM BadPagingException

checkPaging :: Int -> GridField a -> Bool
checkPaging idx gf = maybe False (== idx) si
  where
    si = gf ^? pagingInfo . pgIStartIndex

pagingInfo :: Applicative f => (PagingInfo -> f PagingInfo) -> GridField a -> f (GridField a)
pagingInfo = gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable

setStartIndex :: Int -> GridField a -> GridField a
setStartIndex idx = pagingInfo . pgIStartIndex .~ idx

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
getNumber = takeTill (== '#') *> Data.Attoparsec.Text.take 1 *> takeTill (== ' ')

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
