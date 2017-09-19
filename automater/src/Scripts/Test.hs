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
  ) where 

import Test.QuickCheck hiding (generate)
import qualified Test.QuickCheck as QC
import ClassyPrelude
import Data.Char

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
