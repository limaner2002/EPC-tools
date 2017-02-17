{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Sheets.AggregateSheets where

import ClassyPrelude hiding (throwM)
import Codec.Xlsx
import Control.Lens hiding (cons)
import ParseCSV
import MachineUtils hiding (sampleOn, RowNum, makeRowNum, fromRowNum)
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Char (isDigit)
import Sheets.Core

data AggregateRow = AggregateRow
  { aggLabel :: Text
  , aggNSamples :: Int
  , aggAverage :: Double
  , aggMedian :: Double
  , aggNinetiethPercentLine :: Double
  , aggNinetyFifthPercentLine :: Double
  , aggNinetyNinthPercentLine :: Double
  , aggMinVal :: Double
  , aggMaxVal :: Double
  , aggErrorPct :: Double
  , aggThroughput :: Double
  , aggKbps :: Double
  } | Header
  { headLabel :: Text
  , headNSamples :: Text
  , headAverage :: Text
  , headMedian :: Text
  , headNinetiethPercentLine :: Text
  , headNinetyFifthPercentLine :: Text
  , headNinetyNinthPercentLine :: Text
  , headMinVal :: Text
  , headMaxVal :: Text
  , headErrorPct :: Text
  , headThroughput :: Text
  , headKbps :: Text
  }
  deriving (Show, Eq)

data InvalidAggregateRow = InvalidAggregateRow Text
  deriving Show

instance Exception InvalidAggregateRow

instance ToSheetRow AggregateRow where
  toSheetRow rowNum (Header label nSamples average median ninetiethPercentLine ninetyFifthPercentLine ninetyNinthPercentLine minVal maxVal errorPct throughput kbps) ws =
   ws & cellValueAt (n,1) ?~ (CellText label)
      & cellValueAt (n,2) ?~ (CellText nSamples)
      & cellValueAt (n,3) ?~ (CellText average)
      & cellValueAt (n,4) ?~ (CellText median)
      & cellValueAt (n,5) ?~ (CellText ninetiethPercentLine)
      & cellValueAt (n,6) ?~ (CellText ninetyFifthPercentLine)
      & cellValueAt (n,7) ?~ (CellText ninetyNinthPercentLine)
      & cellValueAt (n,8) ?~ (CellText minVal)
      & cellValueAt (n,9) ?~ (CellText maxVal)
      & cellValueAt (n,10) ?~ (CellText errorPct)
      & cellValueAt (n,11) ?~ (CellText throughput)
      & cellValueAt (n,12) ?~ (CellText kbps)
   where
     n = fromRowNum rowNum
  toSheetRow rowNum (AggregateRow label nSamples average median ninetiethPercentLine ninetyFifthPercentLine ninetyNinthPercentLine minVal maxVal errorPct throughput kbps) ws =
   ws & cellValueAt (n,1) ?~ (CellText label)
      & cellValueAt (n,2) ?~ (CellDouble $ fromIntegral nSamples)
      & cellValueAt (n,3) ?~ (CellDouble $ average)
      & cellValueAt (n,4) ?~ (CellDouble $ median)
      & cellValueAt (n,5) ?~ (CellDouble $ ninetiethPercentLine)
      & cellValueAt (n,6) ?~ (CellDouble $ ninetyFifthPercentLine)
      & cellValueAt (n,7) ?~ (CellDouble $ ninetyNinthPercentLine)
      & cellValueAt (n,8) ?~ (CellDouble $ minVal)
      & cellValueAt (n,9) ?~ (CellDouble $ maxVal)
      & cellValueAt (n,10) ?~ (CellDouble $ errorPct)
      & cellValueAt (n,11) ?~ (CellDouble $ throughput)
      & cellValueAt (n,12) ?~ (CellDouble $ kbps)
   where
     n = fromRowNum rowNum
    -- Reads and returns an aggregate row
  makeRow [l,ns,avg,mdn,ntypct,nfthpct,nnthpct,min,max,error,through,kbps] =
    AggregateRow <$> pure l
                 <*> readVal ns
                 <*> readVal avg
                 <*> readVal mdn
                 <*> readVal ntypct
                 <*> readVal nfthpct
                 <*> readVal nnthpct
                 <*> readVal min
                 <*> readVal max
                 <*> readError error
                 <*> readDouble through
                 <*> readDouble kbps
    where
      readError = readVal . reverse . drop 1 . reverse
      readDouble v = case isPrefixOf "." v of
        True -> readVal $ '0' `cons` v
        False -> readVal v
  makeRow row = throwM $ InvalidAggregateRow $ tshow row <> " is not a valid JMeter aggregate report row."

  makeHead [l,ns,avg,mdn,ntypct,nfthpct,nnthpct,min,max,error,through,kbps] =
    return $ Header l ns avg mdn ntypct nfthpct nnthpct min max error through kbps
  makeHead row = throwM $ InvalidAggregateRow $ tshow row <> " is not a valid JMeter aggregate report header."

filterFcn :: Either SomeException [Text] -> Bool
filterFcn (Left _) = False
filterFcn (Right (x:_)) = checkIt $ headMay x
  where
    checkIt Nothing = False
    checkIt (Just c) = not $ isDigit c

doIt :: MonadResource m =>
  POSIXTime -> 
  ProcessA (Kleisli m) (Event (Text, FilePath)) (Event ())
doIt t = makeWorkbook t myArr >>> addStyleSheet >>> serializeSheet t >>> writeSheet "/tmp/result.xlsx"
  where
    myArr = sourceFile >>> readRows def >>> makeRow_ >>> evMap (id *** fmap asAggRow) >>> makeSheet >>> conditionalFormatting >>> addViewSheet
    asAggRow :: AggregateRow -> AggregateRow
    asAggRow = id


errorFormatting :: Map SqRef ConditionalFormatting
errorFormatting = def & at (SqRef [cell]) ?~ [CfRule (CellIs (OpGreaterThan (Formula "0"))) (Just 0) 1 Nothing]
  where
    cell :: CellRef
    cell = CellRef "J1:J9"

conditionalFormatting :: (ArrowApply a, Functor f) =>
     ProcessA a (Event (f Worksheet)) (Event (f Worksheet))
conditionalFormatting = anytime (arr (fmap (wsConditionalFormattings .~ errorFormatting)))

errorFill :: Fill
errorFill = Fill (Just $ FillPattern (Just errorColor) (Just errorColor) (Just PatternTypeSolid))

errorColor :: Color
errorColor = Color Nothing (Just "ff9999") Nothing Nothing

errorDxf :: Dxf
errorDxf = def & dxfFill .~ (Just errorFill)

styleSheet :: StyleSheet
styleSheet = minimalStyleSheet & styleSheetDxfs .~ [errorDxf]

addStyleSheet :: (ArrowApply a, Functor f) =>
     ProcessA a (Event (f Xlsx)) (Event (f Xlsx))
addStyleSheet = evMap (fmap $ xlStyles .~ renderStyleSheet styleSheet)

createSheet :: IO ()
createSheet = do
  ct <- getPOSIXTime
  runRMachine_ (doIt ct) [("5 Users", "/tmp/aggregate_5.csv"), ("25 Users", "/tmp/aggregate_25.csv"), ("50 Users", "/tmp/aggregate_50.csv"), ("100 Users", "/tmp/aggregate_100.csv")]
