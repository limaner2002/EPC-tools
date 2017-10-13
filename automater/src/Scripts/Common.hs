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

handleValidations :: Either ValidationsException Value -> Appian Value
handleValidations (Right v) = return v
handleValidations (Left ve) = case ve ^. validationsExc . _1 of
  ["You must associate at least one Funding Request"] -> do
    return $ ve ^. validationsExc . _2
  _ -> throwM ve

myLandingPageAction :: Text -> Appian Value
myLandingPageAction actionName = do
  v <- reportsTab
  rid <- getReportId "My Landing Page" v
  v' <- editReport (PathPiece rid)
  form471Link <- handleMissing actionName v' $ v' ^? landingPageLink actionName
  aid <- handleMissing "Action ID" v' $ parseActionId form471Link
  pid <- landingPageAction $ PathPiece aid
  landingPageActionEx $ PathPiece pid

addAllFRNsButtonUpdate :: (Plated s, AsValue s, AsJSON s) => ReifiedMonadicFold m s (Either Text Update)
addAllFRNsButtonUpdate = MonadicFold (failing (getButtonWith addAllFRNsButton . to toUpdate . to Right) (to (const $ Left "Could not find the Add All FRNs button!")))

addAllFRNsButton :: Text -> Bool
addAllFRNsButton label = isPrefixOf "Add all " label && isSuffixOf " FRNs" label

foldGridField :: (b -> a -> Appian b) -> Text -> b -> GridField a -> Appian b
foldGridField f column b gf = do
  let col = gf ^.. gfColumns . at column . traverse
  F.foldlM f b col

foldGridField' :: (b -> GridFieldIdent -> Appian b) -> b -> GridField a -> Appian b
foldGridField' f b gf = do
  let boxes = gf ^.. gfIdentifiers . traverse . traverse
  F.foldlM f b boxes

type Updater = (Text -> ReifiedMonadicFold IO Value (Either Text Update) -> Value -> Appian Value)

    -- Make this use state as soon as the new servant can be used. 
foldGridFieldPagesReport :: ReportId -> ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> Appian (b, Value)) -> b -> Value -> Appian b
foldGridFieldPagesReport rid = foldGridFieldPages_ (sendReportUpdates rid)

foldGridFieldPages :: ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> Appian (b, Value)) -> b -> Value -> Appian b
foldGridFieldPages = foldGridFieldPages_ sendUpdates

foldGridFieldPages_ :: Updater -> ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> Appian (b, Value)) -> b -> Value -> Appian b
foldGridFieldPages_ updateFcn fold f b v = loop b v
  where
    loop accum val = do
      gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
      atomically $ writeTChan logChan $ Msg $ "PagingInfo: " <> (tshow $ gf ^? pagingInfo)
      case nextPage gf gf of
        Nothing -> do
          (accum, _) <- f accum gf
          return accum
        Just _ -> do
          (accum', val') <- f accum gf
          gf' <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
          atomically $ writeTChan logChan $ Msg $ "PagingInfo: " <> (tshow $ gf' ^? pagingInfo)
          loop accum' =<< getNextPage_ updateFcn gf gf' val'

forGridRows_ :: Updater -> (GridField a -> Vector b) -> ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> Value -> Appian Value) -> Value -> Appian Value
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
getPage :: Updater -> ReifiedMonadicFold Appian Value (GridField a) -> Int -> Value -> Appian Value
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

getNextPage_ :: Updater
             -> GridField a -> GridField a -> Value -> Appian Value
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

openReport :: Text -> Appian (ReportId, Value)
openReport reportName = do
  v <- reportsTab
  rid <- getReportId reportName v
  v' <- editReport (PathPiece rid)
  return (rid, v')

    -- This needs to be replaced as soon as ClientM is generalized in servant-client
loggingFunc :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => FilePath -> m ()
loggingFunc fp = S.takeWhile isMsg
  >>> S.map unpackMsg
--   >>> S.writeFile fp
  >>> S.mapM_ (liftIO . print)
  >>> runResourceT
    $ S.repeatM (atomically $ readTChan logChan)
  where
    isMsg (Msg _) = True
    isMsg _ = False
    unpackMsg (Msg t) = unpack t
    unpackMsg _ = error "The impossible happened unpacking the log Message!"

viewRelatedActions :: Value -> RecordRef -> Appian (RecordRef, Value)
viewRelatedActions v recordRef = do
  let ref = PathPiece recordRef
  v' <- viewRecordDashboard ref (PathPiece $ Dashboard "summary")
  dashboard <- handleMissing "Related Actions Dashboard" v' $ v' ^? getRecordDashboard "Related Actions"
  v'' <- viewRecordDashboard ref (PathPiece dashboard)
  return (recordRef, v'')

executeRelatedAction :: Text -> RecordRef -> Value -> Appian Value
executeRelatedAction action recordId val = do
  aid <- PathPiece <$> (handleMissing ("could not find actionId for " <> tshow action) val $ val ^? getRelatedActionId action)
  relatedActionEx (PathPiece recordId) aid

setAgeSort :: GridField a -> GridField a
setAgeSort = gfSelection . traverse . failing _NonSelectable (_Selectable . gslPagingInfo) . pgISort .~ Just [SortField "secondsSinceRequest" True]

selectGridfieldUpdateF :: GridFieldIdent -> GridField a -> ReifiedMonadicFold m s (Either Text Update)
selectGridfieldUpdateF ident gf = MonadicFold (failing (to (const (selectCheckbox ident gf)) . to toUpdate . to Right) (to $ const $ Left "Unable to make the gridfield update"))

selectCheckbox :: GridFieldIdent -> GridField a -> GridField a
selectCheckbox ident = gfSelection . traverse . _Selectable . gslSelected .~ [ident]
