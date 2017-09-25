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
import Control.Lens.Action.Reified
import Scripts.Test

handleValidations :: Either ValidationsException Value -> Appian Value
handleValidations (Right v) = return v
handleValidations (Left ve) = case ve ^. validationsExc . _1 of
  ["You must associate at least one Funding Request"] -> do
    liftIO $ writeFile "/tmp/update.json" $ ve ^. validationsExc . _3 . to (toStrict . encode)
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

getNextPage :: GridField a -> Value -> Appian Value
getNextPage gf = sendUpdates "Next Page" (MonadicFold $ to $ const gf')
  where
    gf' = toUpdate <$> (maybeToEither "This is the last page!" $ nextPage gf)

nextPage :: GridField a -> Maybe (GridField a)
nextPage gf = do
  pi <- gf ^? gfSelection . traverse .gslPagingInfo
  let pi' = pgIStartIndex %~ (+batchSize) $ pi
      batchSize = pi ^. pgIBatchSize
  return $ gfSelection . traverse . gslPagingInfo .~ pi' $ gf
