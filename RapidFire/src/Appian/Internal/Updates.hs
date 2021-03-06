{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Appian.Internal.Updates where

import ClassyPrelude
import Appian.Types
import Appian.Client
import Appian.Internal.Appian
import Appian.Internal.Arbitrary
import Appian.Instances
import Appian.Lens
import Control.Lens
import Control.Lens.Action.Reified
import Data.Aeson
import Data.Aeson.Lens
import Control.Monad.State
import Control.Monad.Trans.Compose
import Control.Monad.Except (MonadError, throwError, catchError)

newtype ActionT m a = ActionT { runActionT :: AppianT m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadState AppianState)

type Action a = forall m. (RapidFire m, MonadGen m) => ActionT m a

newtype ReportT m a = ReportT { runReportT :: (StateT ReportId :.: AppianT) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
    
type Report a = forall m. (RapidFire m, MonadGen m) => ReportT m a

mkReportT :: (ReportId -> AppianT m (a, ReportId)) -> ReportT m a
mkReportT = ReportT . ComposeT . StateT

data DashboardState = DashboardState
  { dashStub :: UrlStub
  , dashRef :: RecordId
  , dashBoard :: Dashboard
  }

newtype DashboardFormT m a = DashboardFormT { runDashboardFormT :: (StateT DashboardState :.: AppianT) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type DashboardForm a = forall m. (RapidFire m, MonadGen m) => DashboardFormT m a

mkDashboardFormT :: (DashboardState -> AppianT m (a, DashboardState)) -> DashboardFormT m a
mkDashboardFormT = DashboardFormT . ComposeT . StateT

class HasUpdate (t :: (* -> *) -> * -> *) (m :: * -> *) where
    update :: RapidFire m => Text -> ReifiedMonadicFold m Value (Either Text Update) -> t m ()

instance HasUpdate ActionT m where
    update msg fold = ActionT $ sendUpdates1 msg fold
    
instance HasUpdate ReportT m where
    update msg fold = mkReportT $ \rid -> do
      sendReportUpdates1 rid msg fold
      pure ((), rid)

instance HasUpdate DashboardFormT m where
  update msg fold = mkDashboardFormT $ \dashState -> do
    sendDashboardUpdates dashState msg fold
    pure ((), dashState)

sendRecordUpdates1 :: RapidFire m => ReportId -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
sendRecordUpdates1 rid msg fold = do
    previousVal <- use appianValue
    newVal <- sendReportUpdates rid msg fold previousVal
    res <- deltaUpdate previousVal newVal
    assign appianValue res

sendDashboardUpdates' :: RapidFire m => DashboardState -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> Value -> AppianT m (Either (ScriptError, Value) Value)
sendDashboardUpdates' (DashboardState urlStub rid dashboard) = sendUpdates_ (dashboardUpdate urlStub rid dashboard)

sendDashboardUpdates :: RapidFire m => DashboardState -> Text -> ReifiedMonadicFold m Value (Either Text Update) -> AppianT m ()
sendDashboardUpdates dashState label f = do
  v <- use appianValue
  eRes <- sendDashboardUpdates' dashState label f v
  case eRes of
    Left (err, newVal) -> do
      assign appianValue newVal
      throwError err
    Right newVal -> assign appianValue newVal

actionByName :: (RapidFire m, MonadGen m) => Text -> Action a -> AppianT m a
actionByName label action = runActionT $ do
    ActionT $ executeActionByName label
    action

execDashboardFormT :: (RapidFire m, MonadGen m) => DashboardFormT m a -> DashboardState -> AppianT m a
execDashboardFormT = evalStateT . getComposeT . runDashboardFormT

runDashboardByName :: RapidFire m => RecordRef -> Text -> AppianT m DashboardState
runDashboardByName rref dashboardName = runDashboardByName' rref dashboardName >>= either throwError pure

runDashboardByName' :: RapidFire m => RecordRef -> Text -> AppianT m (Either ScriptError DashboardState)
runDashboardByName' rref dashboardName = do
  val <- viewRecordDashboard rref (Dashboard "summary")

  summaryHeader <- handleMissing "Could not find embedded header for the summary dashboard" val $ val ^? getEmbeddedHeader

  case summaryHeader ^? hasKeyValue "label" dashboardName . key "link" . key "dashboard" . _String . to Dashboard of
    Nothing -> return $ Left $ MissingComponentError ("Could not find dashboard " <> tshow dashboardName, val)
    Just dashboard -> do

      dashFeed <- viewRecordDashboard rref dashboard

      dashHeader <- handleMissing ("Could not find embedded header for " <> tshow dashboard) dashFeed $ dashFeed ^? getEmbeddedHeader

      uiPart <- handleMissing "Could not find embedded ui for the dashboard" dashFeed $ dashFeed ^? getEmbeddedUi
      -- headerPart <- handleMissing "Could not find embedded header for the dashboard" dashFeed $ dashFeed ^? getEmbeddedHeader
      urlStub <- handleMissing "Could not find the urlstub for the dashboard" summaryHeader $ summaryHeader ^? hasType "RecordListLink" . key "urlstub" . _String . to UrlStub
      let opaqueRecordIdTraversal = deep (key "opaqueRecordId" . _String . to RecordId)
      rrid <- case summaryHeader ^? opaqueRecordIdTraversal of
      	   Just r -> return r
	   Nothing -> case dashHeader ^? opaqueRecordIdTraversal of
	   	   Just r -> return r
		   Nothing -> handleMissing "Could not find the Opaque Record ID" dashHeader Nothing

      assign appianValue uiPart
      return $ Right $ DashboardState urlStub rrid dashboard

execReportT :: RapidFire m => ReportT m a -> ReportId -> AppianT m a
execReportT = evalStateT . getComposeT . runReportT

runReportByName :: RapidFire m => Text -> ReportT m a -> AppianT m a
runReportByName reportName reportFcn = do
  (rid, val) <- openReport reportName
  assign appianValue val
  execReportT reportFcn rid

openReport :: RapidFire m => Text -> AppianT m (ReportId, Value)
openReport reportName = do
  v <- reportsTab
  rid <- getReportId reportName v
  bounds <- use appianBounds
  v' <- thinkTimer bounds $ recordTime ("Edit Report " <> tshow reportName) $ editReport rid
  return (rid, v')
