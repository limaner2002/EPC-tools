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

    -- Make this use state as soon as the new servant can be used. 
foldGridFieldPages :: ReifiedMonadicFold Appian Value (GridField a) -> (b -> GridField a -> Appian (b, Value)) -> b -> Value -> Appian b
foldGridFieldPages fold f b v = loop b v
  where
    loop accum val = do
      gf <- handleMissing "GridField" val =<< (val ^!? runMonadicFold fold)
      atomically $ writeTChan logChan $ Msg $ tshow $ gf ^? gfSelection . traverse . _Selectable . gslPagingInfo
      case nextPage gf gf of
        Nothing -> do
          (accum, _) <- f accum gf
          return accum
        Just _ -> do
          (accum', val') <- f accum gf
          gf' <- handleMissing "GridField" val' =<< (val' ^!? runMonadicFold fold)
          atomically $ writeTChan logChan $ Msg $ tshow $ gf' ^? gfSelection . traverse . _Selectable . gslPagingInfo
          loop accum' =<< getNextPage gf gf' val'

getNextPage :: GridField a -> GridField a -> Value -> Appian Value
getNextPage gf gf' = sendUpdates "Next Page" (MonadicFold $ to $ const gf'')
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
