{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module DriveUI where

import Reflex.Dom
import Reflex.Dom.Xhr
import Scheduler.Google.Types
import Lucid
import Data.Aeson
import Data.Text (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)
import Control.Lens
import Data.Semigroup
import Control.Monad.Ref (Ref)
import GHC.IORef (IORef)
import Prelude
import Common
import Scheduler.Types

type DReq = DriveRequest DFile

data NavType
  = FileNav DFile
  | CrumbNav DFile

fileWidget :: MonadWidget t m => Event t () -> m ()
fileWidget pb = do
  rec resDyn <- browseXhr $ leftmost [DriveRequest Root breadCrumbs <$ pb, evt']
      let files = fmap (^. _Right . _FileList . _1 . getFiles) resDyn
          crumbs = fmap (^. _Right . _FileList . _2 . bcCrumbs) resDyn
          addJobEvt = fmapMaybe (^? _Right . _AddJob) $ updated resDyn
          addJobR = fmap addJobReq addJobEvt
      _ <- performRequestAsync addJobR
      bcDyn <- el "div" $ simpleList crumbs (toDynHtml "div")
      navDyn <- elAttr "table" ("class" =: "pure-table") $ do
        el "thead" $ do
          el "th" $ text "Name"
          el "th" $ text "Type"
        el "tbody" $ simpleList files (toDynHtml "tr")
      let navEvt = switchPromptlyDyn $ fmap (fmap FileNav . leftmost) navDyn
          bcEvt = switchPromptlyDyn $ fmap (fmap CrumbNav . leftmost) bcDyn
          evt = leftmost [navEvt, bcEvt]
          evt' = attachPromptlyDynWith mkReqEvt crumbs evt

  return ()

mkReqEvt :: [DFile] -> NavType -> DReq
mkReqEvt crumbs (FileNav file) = DriveRequest file (bcCrumbs .~ crumbs $ breadCrumbs)
mkReqEvt crumbs (CrumbNav file) = DriveRequest file (bcCrumbs .~ truncated $ breadCrumbs)
  where
    truncated = takeWhile (\f -> f ^. fId /= file ^. fId) crumbs

browseReq :: DReq -> Maybe (XhrRequest Text)
browseReq req
  | typ == Just Folder || file == Root = Just $ xhrRequest "POST" "browse" cfg
  | typ == Just Zip = Just $ xhrRequest "POST" "download" cfg
  | otherwise = Nothing
  where
    cfg = (xhrRequestConfig_headers .~ ("Content-type" =: "application/json"))
      . (xhrRequestConfig_sendData .~ (toStrict . decodeUtf8 $ encode req))
      $ def
    typ = file ^? fType
    file = req ^. reqFile

browseXhr :: MonadWidget t m => Event t DReq -> m (Dynamic t (Either String NavResponse))
browseXhr req = do
  let fileReq = fmapMaybe browseReq req
  evt <- performRequestAsync fileReq
  holdDyn (Right $ FileList (Fetching, breadCrumbs)) $ fmap (^. xhrResponse_responseText . to decodeResponse) evt

decodeResponse :: Maybe Text -> Either String NavResponse
decodeResponse Nothing = Left "Received an empty response from the server."
decodeResponse (Just t) = eitherDecode . encodeUtf8 . fromStrict $ t

addJobReq :: Text -> XhrRequest Text
addJobReq fp = xhrRequest "POST" "addJob1" cfg
  where
    cfg = (xhrRequestConfig_headers .~ ("Content-type" =: "application/json"))
      . (xhrRequestConfig_sendData .~ (toStrict . decodeUtf8 . encode $ fp))
      $ def

toNav :: MonadWidget t m => DFile -> m (Event t DFile)
toNav file = do
  (e, _) <- el' "div" $ text $ file ^. fName
  let evt = domEvent Click e
  return $ file <$ evt

toDynHtml :: MonadWidget t m => Text -> Dynamic t DFile -> m (Event t DFile)
toDynHtml elName d = do
  let d' = fmap (toStrict . renderText . toHtml) d
  e <- elDynHtml' elName d'
  let evt = domEvent Dblclick e

  return $ attachPromptlyDynWith (\x _ -> x) d evt

