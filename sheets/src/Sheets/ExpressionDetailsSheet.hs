{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict #-}

module Sheets.ExpressionDetailsSheet
  ( createSheet
  ) where

import ClassyPrelude hiding (throwM)
import Sheets.Core
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.Trans.Resource
import MachineUtils hiding (fromRowNum)
import ParseCSV (separator)

data ExpressionDetailsRow = ExprRow
  { exprTimestamp :: UTCTime
  , exprName :: Text
  , exprType :: Text
  , exprTotalCount :: Int
  , exprMeanTotalTime :: Double    -- in ms
  , exprMinTotalTime :: Double     -- in ms
  , exprMaxTotalTime :: Double     -- in ms
  } | Header
  { headTimestamp :: Text
  , headName :: Text
  , headType :: Text
  , headTotalCount :: Text
  , headMeanTotalTime :: Text   -- in ms
  , headMinTotalTime :: Text    -- in ms
  , headMaxTotalTime :: Text    -- in ms
  } deriving (Show, Eq)

-- timeFormat = "%e %b %Y %X %Z"
timeFormat = "%F %X %Z"

instance ToSheetRow ExpressionDetailsRow where
  toSheetRow rowNum (Header hTs hName hType hTotal hMean hMin hMax) ws =
    ws & cellValueAt (n,1) ?~ (CellText hTs)
       & cellValueAt (n,2) ?~ (CellText hName)
       & cellValueAt (n,3) ?~ (CellText hType)
       & cellValueAt (n,4) ?~ (CellText hTotal)
       & cellValueAt (n,5) ?~ (CellText hMean)
       & cellValueAt (n,6) ?~ (CellText hMin)
       & cellValueAt (n,7) ?~ (CellText hMax)
    where
      n = fromRowNum rowNum
  toSheetRow rowNum (ExprRow eTs eName eType eTotal eMean eMin eMax) ws =
    ws & cellValueAt (n,1) ?~ (CellText $ renderETs eTs)
       & cellValueAt (n,2) ?~ (CellText eName)
       & cellValueAt (n,3) ?~ (CellText eType)
       & cellValueAt (n,4) ?~ (CellDouble $ fromIntegral eTotal)
       & cellValueAt (n,5) ?~ (CellDouble eMean)
       & cellValueAt (n,6) ?~ (CellDouble eMin)
       & cellValueAt (n,7) ?~ (CellDouble eMax)
    where
      n = fromRowNum rowNum
      renderETs = pack . formatTime defaultTimeLocale timeFormat
  makeRow [ts, name, typ, total, mean, min, max] =
    ExprRow <$> readTime ts
            <*> pure name
            <*> pure typ
            <*> readVal total
            <*> readVal mean
            <*> readVal min
            <*> readVal max
    where
      readTime = parseTimeM True defaultTimeLocale timeFormat . unpack
  makeRow row = throwM $ InvalidSheetRow $ tshow row <> " is not a valid Expression Details row."
  makeHead [ts, name, typ, total, mean, min, max] =
    return $ Header ts name typ total mean min max
  makeHead row = throwM $ InvalidSheetRow $ tshow row <> " is not a valid Expression Details header."

createSheet_ :: MonadResource m =>
  POSIXTime ->
  FilePath ->
  ProcessA (Kleisli m) (Event (Text, FilePath)) (Event ())
createSheet_ t resultPath = makeWorkbook t myArr >>> serializeSheet t >>> writeSheet resultPath
  where
    myArr = sourceFile >>> readRows (def {separator = '\t'}) >>> makeRow_ >>> evMap (id *** fmap asExprRow) >>> makeSheet >>> addViewSheet
    asExprRow :: ExpressionDetailsRow -> ExpressionDetailsRow
    asExprRow = id

createSheet :: FilePath -> FilePath -> IO ()
createSheet dir resultPath = do
  ct <- getPOSIXTime
  runRMachine_ (createSheet_ ct resultPath)$ 
    fmap (id *** ((dir <> "/") <>))
      [ ("node1873", "expressions_details.csv.node1873")
      , ("node1894", "expressions_details.csv.node1894")
      , ("node1895", "expressions_details.csv.node1895")
      , ("node1896", "expressions_details.csv.node1896")
      , ("node1897", "expressions_details.csv.node1897")
      , ("node1898", "expressions_details.csv.node1898")
      , ("node1899", "expressions_details.csv.node1899")
      , ("node1900", "expressions_details.csv.node1900")
      ]
