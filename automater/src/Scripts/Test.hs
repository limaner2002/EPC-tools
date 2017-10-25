{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.Test
  ( arbitraryText
  , arbitraryTextFixed
  , generate
  , arbitraryTextFixedAlphaNum
  , arbitraryTextFixedPrintable
  , QC.resize
  , QC.oneof
  , QC.Gen
  , textFieldArbitrary
  , gridFieldArbitrarySelect
  , paragraphArbitrary
  , paragraphArbitraryUpdate
  , intFieldArbitrary
  , intFieldArbitraryUpdateF
  , dropdownArbitraryUpdateF
  , dropdownArbitraryUpdateF_
  , intFieldArbitraryUpdateF_
  , fillIntField_
  , dropdownArbitrarySelect_
  , QC.getPositive
  , QC.Arbitrary (..)
  , QC.choose
  , QC.shuffle
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

generate :: MonadIO m => Gen a -> m a
generate = liftIO . QC.generate

paragraphArbitraryUpdate :: (MonadIO m, Plated s, AsValue s, AsJSON s) => Text -> Int -> ReifiedMonadicFold m s (Either Text Update)
paragraphArbitraryUpdate label size = MonadicFold (paragraphArbitrary label size)

paragraphArbitrary :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) => Text -> Int -> (Either a Update -> f (Either a Update)) -> s -> f s
paragraphArbitrary label size = getParagraphField label . act (fillParagraph size) . to toUpdate . to Right

fillParagraph :: MonadIO m => Int -> ParagraphField -> m ParagraphField
fillParagraph size pgf = do
  txt <- generate $ arbitraryTextFixedPrintable size
  return $ pgfValue .~ txt $ pgf

gridFieldArbitrarySelect :: MonadIO m => ReifiedMonadicFold m Value (Either Text Update)
gridFieldArbitrarySelect = MonadicFold ( getGridFieldCell
                                       . act (sequence . fmap gfSelect)
                                       . to (fmap toUpdate)
                                       . to resultToEither
                                       )

textFieldArbitrary :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) => Text -> Int -> (Either a Update -> f (Either a Update)) -> s -> f s
textFieldArbitrary label size = getTextField label . act (fillTextField size) . to toUpdate . to Right

fillTextField :: MonadIO m => Int -> TextField -> m TextField
fillTextField size tf = do
  txt <- generate $ arbitraryTextFixedPrintable size
  return $ tfValue .~ txt $ tf

intFieldArbitraryUpdateF_ :: (MonadIO m, Plated s, AsValue s, AsJSON s) => (TextField -> m TextField) -> Text -> ReifiedMonadicFold m s (Either Text Update)
intFieldArbitraryUpdateF_ fillField label = MonadicFold (intFieldArbitrary_ fillField label)

intFieldArbitraryUpdateF :: (MonadIO m, Plated s, AsValue s, AsJSON s) => Text -> ReifiedMonadicFold m s (Either Text Update)
intFieldArbitraryUpdateF = intFieldArbitraryUpdateF_ fillIntField

intFieldArbitrary_ :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) => (TextField -> m TextField) -> Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
intFieldArbitrary_ fillField label = getField
  where
    getField = getTextField label . act fillField . to toUpdate . to Right
    -- err = to (const $ Left $ "Could not find TextField " <> tshow label)

intFieldArbitrary :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) => Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
intFieldArbitrary = intFieldArbitrary_ fillIntField

fillIntField_ :: MonadIO m => Gen Int -> TextField -> m TextField
fillIntField_ gen tf = do
  int <- generate gen
  return $ tfValue .~ (tshow int) $ tf

fillIntField :: MonadIO m => TextField -> m TextField
fillIntField = fillIntField_ QC.arbitrarySizedNatural

gfSelect :: MonadIO m => GridField a -> m (GridField a)
gfSelect gf = do
  case gf ^. gfIdentifiers of
    Nothing -> fail "This grid is not selectable!"
    Just idents -> do
      ident <- generate $ QC.elements $ toList idents
      return $ gfSelection . _Just . _Selectable . gslSelected .~ singleton ident $ gf

dropdownArbitraryUpdateF_ :: (MonadIO m, Plated s, AsValue s, AsJSON s) => (DropdownField -> m DropdownField) -> Text -> ReifiedMonadicFold m s (Either Text Update)
dropdownArbitraryUpdateF_ selectFcn label = MonadicFold (dropdownArbitrary_ selectFcn label)

dropdownArbitraryUpdateF :: (MonadIO m, Plated s, AsValue s, AsJSON s) =>
  Text -> ReifiedMonadicFold m s (Either Text Update)
dropdownArbitraryUpdateF label = MonadicFold (dropdownArbitrary_ dropdownArbitrarySelect label)

dropdownArbitrary_ :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) =>
                     (DropdownField -> m DropdownField) -> Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
dropdownArbitrary_ selectFn label = getIt
  where
    getIt = getDropdown label . act selectFn . to toUpdate . to Right
    err = Left ("Could not find dropdown " <> tshow label)

dropdownArbitrary :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) =>
                     Text -> (Either Text Update -> f (Either Text Update)) -> s -> f s
dropdownArbitrary = dropdownArbitrary_ dropdownArbitrarySelect

dropdownArbitrarySelect_ :: MonadIO m => Gen Int -> DropdownField -> m DropdownField
dropdownArbitrarySelect_ gen df = do
  n <- generate $ gen
  return $ dfValue .~ n $ df

dropdownArbitrarySelect :: MonadIO m => DropdownField -> m DropdownField
dropdownArbitrarySelect df = dropdownArbitrarySelect_ (choose (2, max)) df
  where
    max = df ^. dfChoices . to length
