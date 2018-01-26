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
        sendRecordUpdates1 rid msg fold
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

-- dashboardForm :: (RapidFire m, MonadGen m) => Dashboard a -> DashboardState -> AppianT m a
-- dashboardForm action dashState = execDashboardFormT 

runDashboardByName :: RapidFire m => RecordRef -> Text -> AppianT m DashboardState
runDashboardByName rref dashboardName = do
  val <- viewRecordDashboard rref (Dashboard "summary")
  writeFile "/tmp/summary.json" $ toStrict $ encode val
  dashboard <- handleMissing ("Could not find dashboard " <> tshow dashboardName) val
  	       $ val ^? getEmbeddedHeader . hasKeyValue "label" dashboardName . key "link" . key "dashboard" . _String . to Dashboard
  dashFeed <- viewRecordDashboard rref dashboard
  uiPart <- handleMissing "Could not find embedded ui for the dashboard" dashFeed $ dashFeed ^? getEmbeddedUi
  headerPart <- handleMissing "Could not find embedded header for the dashboard" dashFeed $ dashFeed ^? getEmbeddedHeader
  urlStub <- handleMissing "Could not find the urlstub for the dashboard" headerPart $ headerPart ^? hasType "RecordListLink" . key "urlstub" . _String . to UrlStub
  rrid <- handleMissing "Could not find the Opaque Record ID" headerPart $ headerPart ^? deep (key "opaqueRecordId" . _String . to RecordId)

  assign appianValue uiPart
  return $ DashboardState urlStub rrid dashboard