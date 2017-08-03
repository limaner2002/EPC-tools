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

schedulerWidget :: MonadWidget t m => JobQueue a -> m ()
schedulerWidget jobs = do
  let mp = mapFromList $ zip [1..] (jobs ^. qJobs)
  rec rs <- el "ul" $ listHoldWithKey mp evt toClickable'
      let toEvtMap = Map.mapWithKey (\k e -> fmap (const $ singletonMap k Nothing) e)
          evt = switch . current . fmap (leftmost . Map.elems . toEvtMap) $ rs
  return ()

toClickable :: MonadWidget t m => Int -> Dynamic t Text -> m (Event t Text)
toClickable _ d = do
  (e, _) <- el' "div" $ dynText d
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
