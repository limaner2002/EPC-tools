{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Appian.Internal.Arbitrary
  ( arbitraryText
  , arbitraryTextFixed
--   , generate
  , arbitraryTextFixedAlphaNum
  , arbitraryTextFixedPrintable
  , QC.resize
  , QC.oneof
  , QC.Gen
  , fillTextField
  , textFieldArbitrary
  , textFieldArbitraryF
  , gridFieldArbitrarySelect
  , fillParagraph
  , paragraphArbitrary
  , paragraphArbitraryUpdate
  , paragraphArbitraryUpdateNoError
  , intFieldArbitrary
  , intFieldArbitraryUpdateF
  , dropdownArbitraryUpdateF
  , dropdownArbitraryUpdateF_
  , intFieldArbitraryUpdateF_
  , fillIntField_
  , dropdownArbitrarySelect_
  , dropdownArbitrarySelect
  , QC.getPositive
  , QC.Arbitrary (..)
  , QC.choose
  , QC.shuffle
  , radioArbitrarySelect
  , radioArbitrary
  , radioArbitraryF
  , MonadGen (..)
  , dropdownCidArbitraryUpdateF
  , expressionWidgetArbitrary
  , arbitraryNonZeroNatural
  ) where 

import Test.QuickCheck hiding (generate)
import qualified Test.QuickCheck as QC
import ClassyPrelude
import Data.Char
import Control.Lens hiding (elements)
import Control.Lens.Action
import Control.Lens.Action.Reified
import Appian.Types
import Data.Aeson
import Data.Aeson.Lens
import Appian.Lens
import Appian.Client
import Appian
import Control.Monad.Logger
import Servant.Client

arbitraryText :: Gen Text
arbitraryText = pack <$> (arbitrary :: Gen String)

arbitraryTextFixed :: Int -> Gen Text
arbitraryTextFixed length = pack . take length <$> infiniteList

arbitraryTextFixedAlphaNum :: Int -> Gen Text
arbitraryTextFixedAlphaNum length = pack . take length . fmap alphaNumChar <$> (infiniteList :: Gen [ArbitraryAlphaNum])

arbitraryTextFixedPrintable :: Int -> Gen Text
arbitraryTextFixedPrintable length = pack . take length . fmap printable <$> (infiniteList :: Gen [PrintableChar])

newtype ArbitraryAlphaNum = ArbitraryAlphaNum { alphaNumChar :: Char }
  deriving Show

instance Arbitrary ArbitraryAlphaNum where
  arbitrary = ArbitraryAlphaNum <$> arbitraryAlphaNum

arbitraryAlphaNum :: Gen Char
arbitraryAlphaNum = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

newtype PrintableChar = PrintableChar { printable :: Char }
  deriving Show

instance Arbitrary PrintableChar where
  arbitrary = PrintableChar <$> arbitraryPrintable

arbitraryPrintable :: Gen Char
arbitraryPrintable = arbitrary `suchThat` isPrint
-- arbitraryPrintable = elements $ ['@', '`', '!', 'A', 'a', '"', 'B', 'b', '#', 'C', 'c', '$', 'D', 'd', '%', 'E', 'e', '&', 'F', 'f', '\'', 'G', 'g', '(', 'H', 'h', ')', 'I', 'i', '*', 'J', 'j', '+', 'K', 'k', ',', 'L', 'l', '-', 'M', 'm', '.', 'N', 'n', '/', 'O', 'o', '0', 'P', 'p', '1', 'Q', 'q', '2', 'R', 'r', '3', 'S', 's', '4', 'T', 't', '5', 'U', 'u', '6', 'V', 'v', '7', 'W', 'w', '8', 'X', 'x', '9', 'Y', 'y', ':', 'Z', 'z', ';', '[', '{', '<', '\\', '|', '=', ']', '}', '>', '^', '~', '?', '_']

-- generate :: MonadGen m => Gen a -> m a
-- generate = liftIO . QC.generate

paragraphArbitraryUpdate :: (MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> Int -> ReifiedMonadicFold m s (Either Text Update)
paragraphArbitraryUpdate label size = MonadicFold (paragraphArbitrary label size)

-- | Same as 'paragraphArbitraryUpdate' but will not throw an error if the paragraph is not found
paragraphArbitraryUpdateNoError :: (MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> Int -> ReifiedMonadicFold m s (Either Text Update)
paragraphArbitraryUpdateNoError label size = MonadicFold (paragraphArbitraryNoError label size . act (mapM $ fillParagraph size) . to (fmap toUpdate))

paragraphArbitrary :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> Int -> (Either Text Update -> f (Either Text Update)) -> s -> f s
paragraphArbitrary label size = failing (paragraphArbitraryNoError label size) (to $ const $ Left $ "Could not find paragraph field " <> tshow label) . act (mapM $ fillParagraph size) . to (fmap toUpdate)

-- | Same as 'paragraphArbitrary' but will not throw an error if the paragraph is not found
-- paragraphArbitraryNoError :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> Int -> (Either Text ParagraphField -> f (Either Text ParagraphField)) -> s -> f s
paragraphArbitraryNoError label size = getParagraphField label . to Right

fillParagraph :: MonadGen m => Int -> ParagraphField -> m ParagraphField
fillParagraph size pgf = do
  txt <- genArbitrary $ arbitraryTextFixedPrintable size
  return $ pgfValue .~ txt $ pgf

gridFieldArbitrarySelect :: MonadGen m => ReifiedMonadicFold m Value (Either Text Update)
gridFieldArbitrarySelect = MonadicFold ( getGridFieldCell
                                       . act (sequence . fmap gfSelect)
                                       . to (fmap toUpdate)
                                       . to resultToEither
                                       )

textFieldArbitrary :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> Int -> (Either a Update -> f (Either a Update)) -> s -> f s
textFieldArbitrary label size = getTextField label . act (fillTextField size) . to toUpdate . to Right

textFieldArbitraryF :: MonadGen m => Text -> Int -> ReifiedMonadicFold m Value (Either Text Update)
textFieldArbitraryF label size = MonadicFold (textFieldArbitrary label size)

fillTextField :: MonadGen m => Int -> TextField -> m TextField
fillTextField size tf = do
  txt <- genArbitrary $ arbitraryTextFixedPrintable size
  return $ tfValue .~ txt $ tf

intFieldArbitraryUpdateF_ :: (MonadGen m, Plated s, AsValue s, AsJSON s) => (TextField -> m TextField) -> Text -> ReifiedMonadicFold m s (Either Text Update)
intFieldArbitraryUpdateF_ fillField label = MonadicFold (intFieldArbitrary_ fillField label)

intFieldArbitraryUpdateF :: (MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> ReifiedMonadicFold m s (Either Text Update)
intFieldArbitraryUpdateF = intFieldArbitraryUpdateF_ fillIntField

intFieldArbitrary_ :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) => (TextField -> m TextField) -> Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
intFieldArbitrary_ fillField label = getField
  where
    getField = getTextField label . act fillField . to toUpdate . to Right
    -- err = to (const $ Left $ "Could not find TextField " <> tshow label)

intFieldArbitrary :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
intFieldArbitrary = intFieldArbitrary_ fillIntField

fillIntField_ :: MonadGen m => Gen Int -> TextField -> m TextField
fillIntField_ gen tf = do
  int <- genArbitrary gen
  return $ tfValue .~ (tshow int) $ tf

fillIntField :: MonadGen m => TextField -> m TextField
fillIntField = fillIntField_ QC.arbitrarySizedNatural

gfSelect :: MonadGen m => GridField a -> m (GridField a)
gfSelect gf = do
  case gf ^. gfIdentifiers of
    Nothing -> fail "This grid is not selectable!"
    Just idents -> do
      ident <- genArbitrary $ QC.elements $ toList idents
      return $ gfSelection . _Just . _Selectable . gslSelected .~ singleton ident $ gf

dropdownArbitraryUpdateF_ :: MonadGen m => (DropdownField -> m DropdownField) -> Text -> ReifiedMonadicFold m Value (Either Text Update)
dropdownArbitraryUpdateF_ selectFcn label = MonadicFold (dropdownArbitrary_ (getDropdown label) selectFcn)

dropdownArbitraryUpdateF :: MonadGen m =>
  Text -> ReifiedMonadicFold m Value (Either Text Update)
dropdownArbitraryUpdateF label = MonadicFold (dropdownArbitrary_ (getDropdown label) dropdownArbitrarySelect)

dropdownCidArbitraryUpdateF :: (MonadGen m, Plated s, AsValue s, AsJSON s) =>
                               Text -> ReifiedMonadicFold m s (Either Text Update)
dropdownCidArbitraryUpdateF cid = MonadicFold (dropdownArbitrary_ (dropdownFieldCid cid) dropdownArbitrarySelect)

dropdownArbitrary_ :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) =>
                     LensLike' f s DropdownField -> (DropdownField -> m DropdownField) -> (Either Text Update -> f (Either Text Update)) -> s -> f s
dropdownArbitrary_ lens selectFn = getIt
  where
    getIt = lens . act selectFn . to toUpdate . to Right
--    err = Left ("Could not find dropdown " <> tshow label)

dropdownArbitrary :: (Applicative f, Effective m r f, MonadGen m) =>
                     Text -> (Either Text Update -> f (Either Text Update)) -> Value -> f Value
dropdownArbitrary label = dropdownArbitrary_ (getDropdown label) dropdownArbitrarySelect

dropdownArbitrarySelect_ :: MonadGen m => Gen Int -> DropdownField -> m DropdownField
dropdownArbitrarySelect_ gen df = do
  n <- genArbitrary $ gen
  return $ dfValue .~ n $ df

dropdownArbitrarySelect :: MonadGen m => DropdownField -> m DropdownField
dropdownArbitrarySelect df = dropdownArbitrarySelect_ (choose (2, max)) df
  where
    max = df ^. dfChoices . to length

radioArbitrarySelect_ :: MonadGen m => Gen Int -> RadioButtonField -> m RadioButtonField
radioArbitrarySelect_ gen rf = do
  n <- genArbitrary gen
  return $ rdgValue .~ Just (AppianInteger n) $ rf

radioArbitrarySelect :: MonadGen m => RadioButtonField -> m RadioButtonField
radioArbitrarySelect df = radioArbitrarySelect_ (choose (2, max)) df
  where
    max = df ^. rdgChoices . to length

radioArbitrary_ :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) =>
                     (RadioButtonField -> m RadioButtonField) -> Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
radioArbitrary_ selectFn label = getIt
  where
    getIt = getRadioButtonField label . act selectFn . to toUpdate . to Right
    err = Left ("Could not find Radio Button Field " <> tshow label)

radioArbitrary :: (Applicative f, Effective m r f, MonadGen m, Plated s, AsValue s, AsJSON s) =>
                     Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
radioArbitrary = radioArbitrary_ radioArbitrarySelect

radioArbitraryF :: (MonadGen m, Plated s, AsValue s, AsJSON s) => Text -> ReifiedMonadicFold m s (Either Text Update)
radioArbitraryF label = MonadicFold (radioArbitrary label)

expressionWidgetArbitrary :: (Applicative f, Effective m r f, MonadGen m) => Gen Value -> (Either Text Update -> f (Either Text Update)) -> Value -> f Value
expressionWidgetArbitrary gen = failing (getExpressionInfoPanel . traverse . editor . to Right) (to $ const $ Left $ "Could not find Expression Editor Widget ") . act (mapM $ fillExpression gen) . to (fmap toUpdate)

fillExpression :: MonadGen m => Gen Value -> ExpressionEditorWidget -> m ExpressionEditorWidget
fillExpression gen widget = do
  txt <- toStrict . decodeUtf8 . encode <$> genArbitrary gen
  return $ expwValue .~ txt $ widget

class Monad m => MonadGen m where
  genArbitrary :: Gen a -> m a

instance MonadGen IO where
  genArbitrary = QC.generate

instance (MonadGen m, MonadTrans t, Monad (t m)) => MonadGen (t m) where
  genArbitrary = lift . genArbitrary

-- instance (MonadGen m, Monad m) => MonadGen (AppianT m) where
--   genArbitrary = lift . genArbitrary

-- instance (MonadGen m, Monad m) => MonadGen (LoggingT m) where
--   genArbitrary = lift . genArbitrary

instance MonadGen ClientM where
  genArbitrary = liftIO . genArbitrary

instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = Number . realToFrac <$> (arbitrary :: Gen Float)
    string = String <$> arbitraryText
    array = Array . fromList <$> arbitrary
    object' = Object . mapFromList <$> arbitrary

instance Arbitrary Text where
  arbitrary = arbitraryText

-- instance Arbitrary (HashMap Text Value) where
--   arbitrary = mapFromList _

arbitraryNonZeroNatural :: Integral a => Gen a
arbitraryNonZeroNatural = QC.sized $ \n -> fromInteger <$> QC.choose (1, toInteger n)
