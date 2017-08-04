{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module SchedulerUI where

import Reflex.Dom
import Prelude
import Data.Containers
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text hiding (zip)
import Data.Semigroup
import Control.Monad
import Common
import Scheduler.Types
import Control.Lens
import Data.Aeson
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)

data QAction
  = Fetching
  | Remove Int

schedulerWidget :: (MonadWidget t m, FromJSON a) => Event t () -> (Map Int (Job a) -> Map Int (Job a)) -> m ()
schedulerWidget pb f = do
  rec jq <- jobQueueXhr $ leftmost [Fetching <$ pb, x, Fetching <$ btn]
      let mpEvt = fmap (f . toMpEvt) jq

      x <- elAttr "table" ("class" =: "pure-table") $ do
        el "thead" $ el "tr" $ do
          el "th" $ text "#"
          el "th" $ text "Job Name"
          el "th" $ text "Status"
          el "th" $ text mempty
        el "tbody" $ do
          d <- holdDyn mempty $ mpEvt
          removes <- listViewWithKey d toClickable
          return $ fmap (Remove . Prelude.head . keys) removes
      btn <- pureButton "Refresh"

  return ()

toMpEvt :: JobQueue a -> Map Int (Job a)
toMpEvt jobs = mapFromList $ zip [1..] (jobs ^. qJobs)

toClickable :: MonadWidget t m => Int -> Dynamic t (Job a) -> m (Event t (Job a))
toClickable n d = do
  (e, _) <- el "tr" $ do
    el "td" $ text $ tshow n
    el "td" $ dynText $ fmap (^. jobName) d
    el "td" $ dynText $ fmap (^. jobStatus . to tshow) d
    el' "td" $ el' "a" $ elAttr' "img" ("src" =: "icon/remove") $ pure ()
  let evt = domEvent Click e

  return $ attachPromptlyDynWith (\x _ -> x) d evt

toClickable' :: MonadWidget t m => Int -> Job a -> m (Event t (Maybe Text))
toClickable' n job = do
  (e, _) <- el "tr" $ do
    el "td" $ text $ tshow n
    el "td" $ text $ job ^. jobName
    el "td" $ text $ tshow $ job ^. jobStatus
    el' "td" $ el' "a" $ elAttr' "img" ("src" =: "icon/remove") $ pure ()
  let evt = domEvent Click e

  return $ Nothing <$ evt

jobQueueReq :: XhrRequest ()
jobQueueReq = xhrRequest "GET" "jobQueue" cfg
  where
    cfg = (xhrRequestConfig_headers .~ ("Content-type" =: "application/json")) def

remJobReq :: Int -> XhrRequest ()
remJobReq idx = xhrRequest "GET" ("remJob1?idx=" <> tshow (idx - 1)) def

jobQueueXhr :: (MonadWidget t m, FromJSON a) => Event t QAction -> m (Event t (JobQueue a))
jobQueueXhr evt = do
  qEvt <- performRequestAsync $ fmap makeReq evt
  let x = fmapMaybe (^. xhrResponse_responseText) qEvt
      jobQueue = fmapMaybe (decode . encodeUtf8 . fromStrict) x
  return jobQueue

makeReq :: QAction -> XhrRequest ()
makeReq Fetching = jobQueueReq
makeReq (Remove idx) = remJobReq idx
