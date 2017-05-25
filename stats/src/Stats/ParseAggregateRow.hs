{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import ClassyPrelude
import Data.CSV.Conduit.Conversion
import Control.Lens hiding ((.=))

data AggregateRow = AggregateRow
  { _aggLabel :: Text
  , _aggNSamples :: Int
  , _aggAverage :: Double
  , _aggMedian :: Double
  , _aggNinetyPercentLine :: Double
  , _aggNinetyFifthPercentLine :: Double
  , _aggNinetyNinthPercentLine :: Double
  , _aggMinVal :: Double
  , _aggMaxVal :: Double
  , _aggErrors :: Double
  , _aggErrorPct :: Double
  } deriving (Show, Eq)

makeLenses ''AggregateRow

instance ToNamedRecord AggregateRow where
  toNamedRecord agg = namedRecord
    [ "Label" .= toField (agg ^. aggLabel)
    , "# Samples" .= toField (agg ^. aggNSamples)
    , "Average" .= toField (agg ^. aggAverage)
    , "Median" .= toField (agg ^. aggMedian)
    , "90% Line" .= toField (agg ^. aggNinetyPercentLine)
    , "95% Line" .= toField (agg ^. aggNinetyFifthPercentLine)
    , "99% Line" .= toField (agg ^. aggNinetyNinthPercentLine)
    , "Min" .= toField (agg ^. aggMinVal)
    , "Max" .= toField (agg ^. aggMaxVal)
    , "Errors" .= toField (agg ^. aggErrors)
    , "Error%" .= toField (agg ^. aggErrorPct)
    ]

instance FromNamedRecord AggregateRow where
  parseNamedRecord r = AggregateRow
    <$> r .: "Label"
    <*> r .: "# Samples"
    <*> r .: "Average"
    <*> r .: "Median"
    <*> r .: "90% Line"
    <*> r .: "95% Line"
    <*> r .: "99% Line"
    <*> r .: "Min"
    <*> r .: "Max"
    <*> r .: "Errors"
    <*> r .: "Error%"

instance ToRecord AggregateRow where
  toRecord agg = record
    [ toField $ agg ^. aggLabel
    , toField $ agg ^. aggNSamples
    , toField $ agg ^. aggAverage
    , toField $ agg ^. aggMedian
    , toField $ agg ^. aggNinetyPercentLine
    , toField $ agg ^. aggNinetyFifthPercentLine
    , toField $ agg ^. aggNinetyNinthPercentLine
    , toField $ agg ^. aggMinVal
    , toField $ agg ^. aggMaxVal
    , toField $ agg ^. aggErrors
    , toField $ agg ^. aggErrorPct
    ]

instance FromRecord AggregateRow where
  parseRecord r = AggregateRow
    <$> r .! 0
    <*> r .! 1
    <*> r .! 2
    <*> r .! 3
    <*> r .! 4
    <*> r .! 5
    <*> r .! 6
    <*> r .! 7
    <*> r .! 8
    <*> r .! 9
    <*> r .! 10

-- BSS.readFile >>> parsed (row def) >>> S.concat >>> S.map fromList >>> S.map (runParser . parseRecord) >>> S.map (fmap asAgg) >>> S.concat >>> S.print >>> runResourceT $ "/tmp/aggregate_5.csv"
