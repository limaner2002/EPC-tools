{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Stats.Types where

import ClassyPrelude
import Control.Lens hiding (index, (.=))
import Data.TDigest
import Data.Csv hiding (Only, index, (.!))
import Data.Time.Clock.POSIX
import Sheets
import Sheets.Generic
import Codec.Xlsx (CellValue (..))
import Data.Aeson (ToJSON)

data Stat = Stat
  { _statTime :: UTCTime
  , _statTotal :: Int
  , _statErrors :: Int
  , _statElapsed :: Int
  , _statDigest :: TDigest 25
  , _statMin :: Infinite Int
  , _statMax :: Int
  } deriving Show

data Infinite a = Infinity | Only a
  deriving (Show, Generic)

instance Ord a => Ord (Infinite a) where
  Only _ <= Infinity = True
  Infinity <= Infinity = True
  Infinity <= Only _ = False
  Only a <= Only b = a <= b

instance Eq a => Eq (Infinite a) where
  Infinity == Infinity = True
  Only a == Only b = a == b
  _ == _ = False

dispInfinite :: Show a => Infinite a -> String
dispInfinite Infinity = "Infinity"
dispInfinite (Only n) = show n

instance FromField a => FromField (Infinite a) where
  parseField "Infinity" = pure Infinity
  parseField f = Only <$> parseField f

instance ToField a => ToField (Infinite a) where
  toField Infinity = "Infinity"
  toField (Only n) = toField n

instance ToCellValue a => ToCellValue (Infinite a) where
  toCellValue Infinity = CellDouble (0/0)
  toCellValue (Only n) = toCellValue n

instance ToJSON a => ToJSON (Infinite a)

type Dict = Map Label Stat

newStat :: UTCTime -> Stat
newStat ts = Stat ts 0 0 0 mempty Infinity 0

data HTTPSample = HTTPSample
  { _timeStamp :: UTCTime
  , _elapsed :: Int
  , _label :: Label
  , _responseCode :: ResponseCode
  } deriving Show

newtype Label = Label {_labelVal :: Text}
  deriving (Show, Ord, Eq)

data ResponseCode
  = HTTPResponseCode Int
  | NonHTTPResponseCode Text
  | NoResponseCode
  deriving Show

makeLenses ''HTTPSample
makeLenses ''Label
makePrisms ''ResponseCode
makeLenses ''Stat

data OutputMode
  = FormatTable
  | FormatCSV
  | FormatXlsx

-- instance ToRecord HTTPSample where
--   toRecord s = record
--     [ toField $ s ^. timeStamp
--     , toField $ s ^. elapsed
--     , toField $ s ^. label . labelVal
--     , toField $ s ^. responseCode
--     , toField $ s ^. responseMessage
--     , toField $ s ^. threadName
--     , toField $ s ^. dataType
--     , toField $ s ^. success
--     , toField $ s ^. failureMessage
--     , toField $ s ^. bytes
--     , toField $ s ^. sentBytes
--     , toField $ s ^. grpThreads
--     , toField $ s ^. allThreads
--     , toField $ s ^. latency
--     , toField $ s ^. idleTime
--     , toField $ s ^. connect
--     ]

instance FromNamedRecord HTTPSample where
  parseNamedRecord r = HTTPSample
    <$> (posixSecondsToUTCTime <$> fromInteger <$> (div <$> r .: "timeStamp" <*> pure 1000))
    <*> r .: "elapsed"
    <*> (Label <$> r .: "label")
    <*> (toResponseCode =<< r .: "responseCode")

instance FromRecord HTTPSample where
  parseRecord r = HTTPSample
    <$> (posixSecondsToUTCTime <$> fromInteger <$> (div <$> r .! 0 <*> pure 1000))
    <*> r .! 1
    <*> (Label <$> r .! 2)
    <*> (toResponseCode =<< r .! 3)
    -- <*> r .! 4
    -- <*> r .! 5
    -- <*> r .! 6
    -- <*> (toBool =<< r .! 7)
    -- <*> r .! 8
    -- <*> r .! 9
    -- <*> r .! 10
    -- <*> r .! 11
    -- <*> r .! 12
    -- <*> r .! 13
    -- <*> r .! 14
    -- <*> r .! 15

toResponseCode :: Text -> Parser ResponseCode
toResponseCode txt
  | onull txt = return NoResponseCode
  | isPrefixOf "Non HTTP response code: " txt = return $ NonHTTPResponseCode txt
  | otherwise = case HTTPResponseCode <$> readMay txt of
      Nothing -> fail "Not a valid HTTP Response Code"
      Just rc -> return rc

toBool :: Text -> Parser Bool
toBool "false" = return False
toBool "true" = return True
toBool x = fail $ show x <> " is not a valid Boolean value."

(.!) :: FromField a => Record -> Int -> Parser a
r .! n = case index r n of
  Nothing -> fail $ "Index out of bounds for record: " <> show r
  Just f -> parseField f

data AggregateRow = AggregateRow
  { _aggLabel :: Text
  , _aggNSamples :: Int
  , _aggAverage :: Double
  , _aggMedian :: Double
  , _aggNinetyPercentLine :: Double
  , _aggNinetyFifthPercentLine :: Double
  , _aggNinetyNinthPercentLine :: Double
  , _aggMinVal :: Infinite Int
  , _aggMaxVal :: Int
  , _aggErrors :: Int
  , _aggErrorPct :: Double
  }
  deriving (Show, Eq, Generic)

makeLenses ''AggregateRow
-- makePrisms ''AggregateRow

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

instance ToSheetRow AggregateRow
instance ToJSON AggregateRow
instance ToCellValue a => ToSheetRow [a] where
  toSheetRow = fmap toCellValue

statToAggregateRow :: Label -> Stat -> AggregateRow
statToAggregateRow statLabel stat = AggregateRow
    (statLabel ^. labelVal)
    (stat ^. statTotal)
    avg
    (fromMaybe (0/0) $ median dg)
    (fromMaybe (0/0) $ quantile 0.9 dg)
    (fromMaybe (0/0) $ quantile 0.95 dg)
    (fromMaybe (0/0) $ quantile 0.99 dg)
    (stat ^. statMin)
    (stat ^. statMax)
    (stat ^. statErrors)
    pctg
  where
    pctg :: Double
    pctg = fromIntegral (stat ^. statErrors) / fromIntegral (stat ^. statTotal) * 100
    avg :: Double
    avg = fromIntegral (stat ^. statElapsed) / fromIntegral (stat ^. statTotal)
    dg = stat ^. statDigest

data RecordLensTest a = R
  { _a :: Text
  , _b :: Text
  }
  | H
  { _c :: a
  }
  deriving (Show, Generic)

instance ToCellValue a => ToSheetRow (RecordLensTest a)
