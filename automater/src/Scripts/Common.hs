{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.Common where

import ClassyPrelude
import Control.Lens
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
      atomically $ writeTChan logChan $ Msg $ "PagingInfo: " <> (tshow $ gf ^? gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable)
      case nextPage gf gf of
        Nothing -> do
          (accum, _) <- f accum gf
          return accum
        Just _ -> do
          (accum', val') <- f accum gf
          gf' <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
          atomically $ writeTChan logChan $ Msg $ "PagingInfo: " <> (tshow $ gf' ^? gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable)
          loop accum' =<< getNextPage_ updateFcn gf gf' val'

getNextPage_ :: Updater
             -> GridField a -> GridField a -> Value -> Appian Value
getNextPage_ updateFcn gf gf' = updateFcn "Next Page" (MonadicFold $ to $ const gf'')
  where
    gf'' = toUpdate <$> (maybeToEither "This is the last page!" $ nextPage gf gf')

nextPage :: GridField a -> GridField a -> Maybe (GridField a)
nextPage gf gf' = do
  pi <- gf ^? gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable
  let pi' = pgIStartIndex %~ (+batchSize) $ pi
      batchSize = pi ^. pgIBatchSize
  case pi' ^. pgIStartIndex > gf ^. gfTotalCount of
    True -> Nothing
    False -> return $ gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable .~ pi' $ gf'

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
