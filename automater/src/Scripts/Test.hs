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

intFieldArbitrary :: (Applicative f, Effective m r f, MonadIO m, Plated s, AsValue s, AsJSON s) => Text -> (Either a Update -> f (Either a Update)) -> s -> f s
intFieldArbitrary label = getTextField label . act fillIntField . to toUpdate . to Right

fillIntField :: MonadIO m => TextField -> m TextField
fillIntField tf = do
  int <- generate (QC.arbitrarySizedNatural :: Gen Int)
  return $ tfValue .~ (tshow int) $ tf

gfSelect :: MonadIO m => GridField a -> m (GridField a)
gfSelect gf = do
  case gf ^. gfIdentifiers of
    Nothing -> fail "This grid is not selectable!"
    Just idents -> do
      ident <- generate $ QC.elements idents
      return $ gfSelection . gslSelected .~ [ident] $ gf
