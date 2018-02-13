{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scripts.FCCForm471Types where

import Control.Lens
import ClassyPrelude
import qualified Data.Csv as Csv
import Appian.Instances
import Scripts.Common (HasLogin (..), SelectOrgMethod (..))
import qualified Data.Attoparsec.Text as T
import Appian.Types
import Appian.Lens
import Appian
import Data.Aeson
import Data.Aeson.Lens
import qualified Test.QuickCheck as QC
import Appian.Internal.Arbitrary
import Scripts.Common
import Appian.Client
import Formatting

data Form471Conf = Form471Conf
  { _nFRNs :: Int
  , _nLineItems :: Int
  , _spin :: Text
  , _category :: Category
  , _applicant :: Login
  , _lineItemSize :: LineItemSize
  , _form470Search :: Form470SearchType
  , _createFRNType :: CreateFRNType
  , _selectOrgMethod :: SelectOrgMethod
  } deriving Show

instance Csv.FromNamedRecord Form471Conf where
  parseNamedRecord r = Form471Conf
    <$> r Csv..: "nFRNs"
    <*> r Csv..: "nLineItems"
    <*> r Csv..: "spin"
    <*> r Csv..: "category"
    <*> Csv.parseNamedRecord r
    <*> r Csv..: "lineItemSize"
    <*> Csv.parseNamedRecord r
    <*> r Csv..: "Create FRN Mode"
    <*> r Csv..: "Select Org Method"

data Form470SearchType
  = ByBEN Text
  | By470 Text
  | No470
  deriving Show

instance Csv.FromNamedRecord Form470SearchType where
  parseNamedRecord r =
    ByBEN <$> r Csv..: "BEN to Copy 470"
    <|> By470 <$> r Csv..: "470"
    <|> pure No470

instance Csv.ToNamedRecord Form470SearchType where
  toNamedRecord (ByBEN t) = Csv.namedRecord
    [ ("BEN to Copy 470", encodeUtf8 t)
    ]
  toNamedRecord (By470 t) = Csv.namedRecord
    [ ("470", encodeUtf8 t)
    ]
  toNamedRecord No470 = mempty

data Form470FunctionType
  = BMIC
  deriving (Show, Eq)

data LineItemSize
  = Small
  | Regular
  | Large
  | Range Int Int
  deriving (Show, Read)

data Category
  = Cat1
  | Cat2

instance Show Category where
  show Cat1 = "Category 1"
  show Cat2 = "Category 2"

instance Csv.FromField LineItemSize where
  parseField bs = case readMay (decodeUtf8 bs) of
    Nothing -> fail $ show bs <> " does not appear to be a valid Line Item Size. Use only 'Small' or 'Regular'"
    Just size -> return size

instance Csv.FromField Category where
  parseField bs = case decodeUtf8 bs of
    "1" -> return Cat1
    "2" -> return Cat2
    _ -> fail $ show bs <> " does not appear to be a valid category. Use only '1' or '2'"

data CreateFRNType
  = NewFRN
  | CopyFRN SearchFRNMethod
  deriving (Show, Eq)

instance Csv.FromField CreateFRNType where
  parseField bs = either fail pure $ parseResult
    where
      parseResult = T.parseOnly parseCreateFRNType $ decodeUtf8 bs
      parseCreateFRNType
        = T.string "New" *> pure NewFRN
        <|> T.string "Copy " *> (CopyFRN <$> parseFRNMethod)

data SearchFRNMethod
  = ByFCCForm471 Int
  | ByFRNNumber Int
  deriving (Show, Eq)

instance Csv.FromField SearchFRNMethod where
  parseField bs = either fail pure $ parseResult
    where
      parseResult = T.parseOnly parseFRNMethod $ decodeUtf8 bs

parseFRNMethod
  = T.string "471 " *> (ByFCCForm471 <$> T.decimal)
  <|> T.string "FRN " *> (ByFRNNumber <$> T.decimal)
  <|> fail "Could not decode SearchFRNMethod"

data DiscountRates = DiscountRates
  { _discCat1 :: DiscountRate
  , _discCat2 :: DiscountRate
  , _discVoice :: DiscountRate
  } deriving Show

getDiscountRate :: Fold Value DiscountRates
getDiscountRate = getGridFieldCell . traverse . gfColumns
  . runFold (DiscountRates
             <$> Fold (at "Category One Discount Rate" . traverse . _TextCell . traverse . traverse . to parseDiscountRate . traverse)
             <*> Fold (at "Category Two Discount Rate" . traverse . _TextCell . traverse . traverse . to parseDiscountRate . traverse)
             <*> Fold (at "Voice Discount Rate" . traverse . _TextCell . traverse . traverse . to parseDiscountRate . traverse)
            )

newtype DiscountRate = DiscountRate Int
  deriving Show

parseDiscountRate :: Text -> Either String DiscountRate
parseDiscountRate = T.parseOnly discountRateParser

discountRateParser :: T.Parser DiscountRate
discountRateParser = DiscountRate <$> T.decimal <* T.char '%'
  T.<?> "Failed to parse the discount rate"

data MonthlyCost = MonthlyUnitCost
  { _mthCost :: DollarAmt
  , _mthIneligible :: DollarAmt
  , _mthEligible :: EligibleCost
  , _mthQuantity :: Int
  , _mthMonthsOfService :: Int
  } deriving Show

data OneTimeCost = OneTimeUnitCost
  { _otCost :: DollarAmt
  , _otIneligible :: DollarAmt
  , _otEligible :: EligibleCost
  , _otQuantity :: Int
  } deriving Show

data LineItemCost = LineItemCost
  { _lineMonthly :: MonthlyCost
  , _lineOneTime :: OneTimeCost
  } deriving Show

data EligibleCost
  = Calculated
  | EligibleCost DollarAmt
  deriving (Read, Show)

instance FromJSON EligibleCost where
  parseJSON (String "") = pure Calculated
  parseJSON (String amt) = case EligibleCost <$> (readMay =<< stripPrefix "= $" amt) of
    Nothing -> fail $ unpack amt <> " is not a valid cost."
    Just cost -> return cost

newtype DollarAmt = DollarAmt { _unDollar :: Double }
  deriving (Show, Num, Floating, Fractional, Ord, Eq, Read)

instance FromJSON DollarAmt where
  parseJSON (String amt) = case DollarAmt <$> (readMay =<< stripPrefix "$" amt) of
    Nothing -> fail $ unpack amt <> " is not a valid Dollar amount."
    Just dollarAmt -> pure dollarAmt

instance QC.Arbitrary DollarAmt where
  arbitrary = DollarAmt <$> arbitrary

instance QC.Arbitrary MonthlyCost where
  arbitrary = do
    cost <- QC.getPositive <$> arbitrary
    ineligible <- QC.getPositive <$> arbitrary
    quantity <- QC.getPositive <$> arbitrary
    let eligibleCost = EligibleCost ((cost - ineligible) * (DollarAmt $ fromIntegral quantity))

    pure $ MonthlyUnitCost cost ineligible eligibleCost quantity 12

instance QC.Arbitrary OneTimeCost where
  arbitrary = do
    cost <- QC.getPositive <$> arbitrary
    ineligible <- QC.getPositive <$> arbitrary
    quantity <- QC.getPositive <$> arbitrary
    let eligibleCost = EligibleCost ((cost - ineligible) * (DollarAmt $ fromIntegral quantity))

    pure $ OneTimeUnitCost cost ineligible eligibleCost quantity

data BandwidthSpeeds = BandwidthSpeeds
  { _bdwDownload :: Int
  , _bdwDownUnits :: BWUnits
  , _bdwUpload :: Int
  , _bdwUpUnits :: BWUnits
  , _bdwBurstable :: Burstable
  } deriving Show

data BWUnits
  = Gbps
  | Mbps
  | BWother
  deriving (Show, Eq)

instance Parseable BWUnits where
  parseElement "Gbps" = Right Gbps
  parseElement "Mbps" = Right Mbps
  parseElement _ = Right BWother

data Burstable = Burstable
  { _burstMax :: Int
  , _burstUnit :: BWUnits
  }
  | NotBurstable
  deriving Show

instance IsInput Burstable where
  enterInput (Burstable bm bu) = do
    sendUpdates1 "Select 'Yes' for Burstable" (buttonUpdateF "Yes")
    sendUpdates1 "Enter max speed" (textUpdateF "What is the maximum burstable speed?" (tshow bm))
    sendUpdates1 "What is the unit of the burstable speed?" (dropdownUpdateF' "What is the unit of the burstable speed?" bu)
  enterInput NotBurstable = sendUpdates1 "Select 'No' for Burstable" (buttonUpdateF "No")

instance QC.Arbitrary BWUnits where
  arbitrary = QC.oneof [pure Gbps, pure Mbps]

instance QC.Arbitrary Burstable where
  arbitrary = Burstable <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary BandwidthSpeeds where
  arbitrary = BandwidthSpeeds
    <$> arbitraryNonZeroNatural
    <*> QC.arbitrary
    <*> arbitraryNonZeroNatural
    <*> QC.arbitrary
    <*> QC.arbitrary

makeLenses ''Form471Conf
makeLenses ''DiscountRate
makeLenses ''MonthlyCost
makeLenses ''OneTimeCost
makeLenses ''LineItemCost
makeLenses ''BandwidthSpeeds
makeLenses ''DollarAmt

makePrisms ''Form470SearchType
makePrisms ''Category
makePrisms ''LineItemSize
makePrisms ''CreateFRNType
makePrisms ''EligibleCost

instance QC.Arbitrary LineItemCost where
  arbitrary = QC.suchThat lineItemCost checkLineItemCost
    where
      lineItemCost = LineItemCost <$> arbitrary <*> arbitrary

checkLineItemCost :: LineItemCost -> Bool
checkLineItemCost lineItem = checkCosts (lineItem ^. lineMonthly) (lineItem ^. lineOneTime)

checkCosts :: MonthlyCost -> OneTimeCost -> Bool
checkCosts monthly oneTime = checkMonthlyEligible && checkOneTimeEligible && checkPreDiscountEligible
  where
    oneTimeEligible = oneTime ^? otEligible . _EligibleCost
    monthlyEligible = monthly ^? mthEligible . _EligibleCost
    checkMonthlyEligible = monthlyEligible > Just 0
    checkOneTimeEligible = oneTimeEligible > Just 0
    checkPreDiscountEligible = ((+) <$> oneTimeEligible <*> monthlyEligible) < Just 9999999999.99

instance IsInput BandwidthSpeeds where
  enterInput bdw = do
    isEditable <- usesValue (has $ getTextField "Bandwidth Download Speed" . tfReadOnly . _Just . only True)
    case isEditable of
      False -> do
        sendUpdates1 "Enter 'Bandwidth Download Speed'" (textUpdateF "Bandwidth Download Speed" (tshow $ bdw ^. bdwDownload))
        sendUpdates1 "Enter 'Download Units'" (dropdownUpdateF' "Bandwidth Download Units" (bdw ^. bdwDownUnits))
        sendUpdates1 "Ether 'Bandwidth Upload Speed'" (textUpdateF "Bandwidth Upload Speed" (tshow $ bdw ^. bdwUpload))
        sendUpdates1 "Enter 'Upload Units'" (dropdownUpdateF' "Bandwidth Upload Units" (bdw ^. bdwUpUnits))
      True -> return ()
    enterInput (bdw ^. bdwBurstable)

instance HasLogin Form471Conf where
  getLogin conf = conf ^. applicant

instance Csv.ToNamedRecord Form471Conf where
  toNamedRecord conf = Csv.namedRecord
    [ ("nFRNs", conf ^. nFRNs . to (encodeUtf8 . tshow))
    , ("nLineItems", conf ^. nLineItems . to (encodeUtf8 . tshow))
    , ("spin", conf ^. spin . to encodeUtf8)
    , ("category", conf ^. category . to (encodeUtf8 . tshow))
    , ("username", conf ^. applicant . username . to encodeUtf8)
    , ("password", conf ^. applicant . password . to encodeUtf8)
    , ("lineItemSize", conf ^. lineItemSize . to (encodeUtf8 . tshow))
    ]
    <> Csv.toNamedRecord (conf ^. form470Search)

instance IsInput LineItemCost where
  enterInput line = do
    let dispDollarAmt = unDollar . to (format $ fixed 2) . to toStrict
    sendUpdates1 "Enter 'Monthly Recurring Cost'" (textFieldCidUpdateF "ee957a1e3a2ca52198084739fbb47ba3"
                                                   (line ^. lineMonthly . mthCost . dispDollarAmt)
                                                  )
    sendUpdates1 "Enter 'Monthly Recurring Ineligible Costs'" (textFieldCidUpdateF "caeb5787e0d7c381e182e53631fb57ab"
                                                               (line ^. lineMonthly . mthIneligible . dispDollarAmt)
                                                              )
    sendUpdates1 "Enter 'One-Time Cost'" (textFieldCidUpdateF "a20962004cc39b76be3d841b402ed5cc"
                                          (line ^. lineOneTime . otCost . dispDollarAmt)
                                         )
    sendUpdates1 "Enter 'One-Time Ineligible Cost'" (textFieldCidUpdateF "3664d88f53b3b462acdfebcb53c93b1e"
                                                     (line ^. lineOneTime . otIneligible . dispDollarAmt)
                                                    )
