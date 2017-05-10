{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StreamParser where

import ClassyPrelude hiding (throwM)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Types as A

data CSVSettings = CSVSettings
  { separator :: Char
  , csvQuote :: Char
  , newlineChar :: Char
  } deriving Show

defaultCSVSettings :: CSVSettings
defaultCSVSettings = CSVSettings ',' '"' '\n'

parseRow csvSettings = manyTill' (parseCell csvSettings) (endOfLine <|> endOfInput)

                       -- Does not parse escaped quotes correctly. Returns the two quote chars instead of the individual character.
parseCell :: CSVSettings -> A.Parser ByteString ByteString
parseCell csvSettings = parseQuoted <|> parseValue
  where
    parseQuoted = parseQuote csvSettings *> takeTill (\c -> c == csvQuote csvSettings || isEndOfLine (fromChar c)) <* (parseQuote csvSettings >> (optional $ char (separator csvSettings)))
    parseValue = takeTill (\c -> c == separator csvSettings || isEndOfLine (fromChar c)) <* (optional $ char (separator csvSettings))

parseQuote :: CSVSettings -> Parser Char
parseQuote csvSettings = do
  c1 <- char quoteChar
  mC2 <- peekChar
  case mC2 of
    Nothing -> return c1
    Just c2 -> case c2 == quoteChar of
      True -> fail "Quoted!"
      False -> return c1
  where
    quoteChar = csvQuote csvSettings

fromChar :: Char -> Word8
fromChar = toEnum . fromEnum

toChar :: Word8 -> Char
toChar = toEnum . fromEnum

