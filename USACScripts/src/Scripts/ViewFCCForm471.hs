{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ClassyPrelude
import Appian
import Appian.Instances
import Appian.Client
import Appian.Types
import Appian.Lens
import Data.Aeson
import Control.Lens hiding (Index)
import Scripts.Opts
import Scripts.Test
import Stats.CsvStream

viewFCCForm471 :: (RapidFire m, MonadGen m) => Login -> AppianT m Value
viewFCCForm471 _ = do
  viewRecordByName "FCC Forms 471"
  sendRecordUpdates "Select 'Incomplete' Status" (dropdownUpdateF "Status" 2)
  sendRecordUpdates "Select Funding Year 2018" (dropdownUpdateF "Funding Year" 2)
  mGw <- usesValue (^? getGridWidget)
  case mGw of
    Nothing -> fail "Could not find grid widget!"
    Just gw -> do
      mV <- testFcn gw
      case mV of
        Nothing -> fail "Could not view the record dashboard for some reason!"
        Just v -> return v

  -- use appianValue

runViewFCCForm471 :: Bounds
                           -> HostUrl
                           -> LogMode
                           -> CsvPath
                           -> RampupTime
                           -> NThreads
                           -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runViewFCCForm471 = runIt viewFCCForm471

                    -- Does not page yet
arbitraryGridRow :: (RapidFire m, MonadGen m) => (IsSequence row, Index row ~ Int, Element row ~ b) => (GridWidget a -> row) -> (b -> AppianT m c) -> GridWidget a -> AppianT m c
arbitraryGridRow getColumn f gw = do
  x <- genArbitrary $ choose (0, min 49 (gw ^. gwTotalCount - 1))
  let column = getColumn gw
      row = indexEx column x
  f row

newtype ColumnName = ColumnName Text
  deriving (Show, Eq, IsString)

arbitraryGridRowByColName :: (RapidFire m, MonadGen m) => ColumnName -> (a -> AppianT m b) -> GridWidget a -> AppianT m b
arbitraryGridRowByColName (ColumnName colName) f = arbitraryGridRow (^.. gwVal . traverse . _2 . at colName . traverse) f

testFcn :: (RapidFire m, MonadGen m) => GridWidget GridWidgetCell -> AppianT m (Maybe Value)
testFcn gw = arbitraryGridRowByColName "FCC Form 471 Number" (\c -> sequence $ viewRecordDashboard <$> (c ^? _GWLink . traverse . _RecordLink . _2) <*> pure (Dashboard "summary")) gw

writeResponsesWithErrors :: MonadIO m => FilePath -> [Maybe (Either ServantError (Either ScriptError Value))] -> m ()
writeResponsesWithErrors _ [] = return ()
writeResponsesWithErrors fp l = mapM_ (uncurry (writeResponseWithErrors fp)) $ zip [1..] l

writeResponseWithErrors :: MonadIO m => FilePath -> Int -> Maybe (Either ServantError (Either ScriptError Value)) -> m ()
writeResponseWithErrors _ _ Nothing = putStrLn "Nothing was provided to this thread."
writeResponseWithErrors _ _ (Just (Left serverError)) = print serverError
writeResponseWithErrors _ _ (Just (Right (Left scriptError))) = print scriptError
writeResponseWithErrors fp i (Just (Right (Right val))) = writeFile (fp <> "_" <> show i <> ".json") $ toStrict $ encode $ val
