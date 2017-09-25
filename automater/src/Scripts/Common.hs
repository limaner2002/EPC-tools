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

-- foldGridField :: (b -> GridField a -> m b) -> m b -> FoldM m (GridField a) b
-- foldGridField = 

-- getAllPages :: ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> b) -> b -> Value -> Appian b
-- getAllPages fold f b v = loop accum
--   where
--     loop accum = do
--       gf <- v ^!! runMonadicFold fold
--       case nextPage gf of
--         Nothing -> return $ f accum v
--         Just gf' -> do
          
foldGridField :: ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> Appian b) -> b -> Value -> Appian b
foldGridField fold f b v = loop b v
  where
    loop accum val = do
      gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
      case nextPage gf of
        Nothing -> f accum gf
        Just _ -> do
          liftIO $ putStrLn $ "Getting the next page!"
          accum' <- f accum gf
          loop accum' =<< getNextPage gf val

getNextPage :: GridField a -> Value -> Appian Value
getNextPage gf = sendUpdates "Next Page" (MonadicFold $ to $ const gf')
  where
    gf' = toUpdate <$> (maybeToEither "This is the last page!" $ nextPage gf)

nextPage :: GridField a -> Maybe (GridField a)
nextPage gf = do
  pi <- gf ^? gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable
  let pi' = pgIStartIndex %~ (+batchSize) $ pi
      batchSize = pi ^. pgIBatchSize
  case pi' ^. pgIStartIndex > gf ^. gfTotalCount of
    True -> Nothing
    False -> return $ gfSelection . traverse . failing (_Selectable . gslPagingInfo) _NonSelectable .~ pi' $ gf
