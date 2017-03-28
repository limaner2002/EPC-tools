{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parsers where

import ClassyPrelude hiding (take, try)
import Data.Aeson (json, Value(..))

import Data.Attoparsec.ByteString.Char8
-- import Data.ByteString.Internal (c2w)

appianResponseParser :: Parser Value
appianResponseParser = (html >> pure Null) <|> json -- <|> (spaces >> endOfInput >> pure Null)

spaces :: Parser ()
spaces = skipWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

html :: Parser ByteString
html = spaces *> stringCI "<!DOCTYPE html>" *> body
  where
    body = takeTill (\c -> c == '<') *> (stringCI "</html>" <|> (take 1 >> body))
