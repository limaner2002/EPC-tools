{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.FCCForm471Certification where

import ClassyPrelude hiding (takeWhile, take)
import Control.Lens hiding (index)
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens hiding (checkResult)
import Appian.Client
import Scripts.Test
import qualified Test.QuickCheck as QC
import Control.Lens.Action
import Control.Lens.Action.Reified
import Scripts.Common
import Control.Monad.Logger
import Scripts.FCCForm471Common
import Data.Attoparsec.Text
import Control.Monad.Time
import qualified Data.Csv as Csv
import Data.Random (MonadRandom)
import Control.Monad.Except

data CertConf = CertConf
  { _certFormNum :: Form471Num
  , _certLogin :: Login
  } deriving Show

makeLenses ''CertConf

instance Csv.FromNamedRecord CertConf where
  parseNamedRecord r = CertConf
    <$> r Csv..: "formNum"
    <*> Csv.parseNamedRecord r

instance HasLogin CertConf where
  getLogin conf = conf ^. certLogin

form471Certification :: (RunClient m, MonadThrow m, MonadTime m, MonadLogger m, MonadCatch m, MonadBase IO m, MonadRandom m, MonadError ServantError m) => CertConf -> AppianT m Form471Num
form471Certification conf = do
  tasksList <- tasksTab Nothing
  taskId <- handleMissing ("Cannot find task for " <> tshow (conf ^. certFormNum)) tasksList $ tasksList ^? hasKeyValueWith (reviewTask conf) "content" . key "id" . _String . to (stripPrefix "t-") . traverse . to TaskId
  taskAccept taskId
  taskStatus taskId
    >>= sendUpdates "Check box and Continue to Certification" (selectAllCheckboxesUpdateF
                                                              <|> MonadicFold (to $ buttonUpdate "Continue to Certification")
                                                              )
    >>= sendUpdates "Check all boxes, Select dropdowns, and Click Certify" (selectAllCheckboxesUpdateF
                                                                           <|> selectAllDropdownsUpdateF
                                                                           <|> MonadicFold (to $ buttonUpdate "Certify")
                                                                           )
    >>= checkResult
  
parse471Number :: Parser Form471Num
parse471Number = takeWhile (/= '#') *> take 1 *> (Form471Num <$> decimal)

reviewTask :: CertConf -> Text -> Bool
reviewTask conf t = either (const False) (== conf ^. certFormNum) $ parsedNum
  where
    parsedNum = parseOnly parse471Number t

selectAllCheckboxesUpdateF :: (Plated s, AsValue s, AsJSON s) => ReifiedMonadicFold m s (Either Text Update)
selectAllCheckboxesUpdateF = MonadicFold (failing (hasType "CheckboxField" . _JSON . to (cbfValue .~ Just [1]) . to toUpdate . to Right) (to $ const $ Left "Could not find any checkboxes!"))

selectAllDropdownsUpdateF :: (Plated s, AsValue s, AsJSON s) => ReifiedMonadicFold m s (Either Text Update)
selectAllDropdownsUpdateF = MonadicFold (failing (hasType "DropdownField" . _JSON . to (dfValue .~ 3) . to toUpdate . to Right) (to $ const $ Left "Could not find any dropdowns!"))

checkResult :: (MonadThrow m, RunClient m, MonadTime m, MonadLogger m, MonadCatch m, MonadBase IO m, MonadRandom m, MonadError ServantError m) => Value -> AppianT m Form471Num
checkResult v = do
  num <- handleMissing "It appears the 471 was not created successfully?" v $ v ^? hasKeyValueWith (isPrefixOf "You have successfully filed FCC Form 471") "label" . key "label" . _String . to (parseOnly parse471Number) . traverse
  sendUpdates "Click Close" (MonadicFold $ to $ buttonUpdate "Close") v
  return num

