{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sheets.Opts
  ( sheetsInfo
  ) where

import ClassyPrelude
import Sheets.ExpressionDetailsSheet
import Options.Applicative

sheetsOptParser :: Parser (IO ())
sheetsOptParser = createSheet
  <$> strOption
  (  long "sourceDir"
  <> short 's'
  <> metavar "SOURCE_DIR"
  <> help "The directory which holds the log files to read"
  )
  <*> strOption
  (  long "outputFile"
  <> short 'o'
  <> metavar "OUTPUT_FILE"
  <> help "The path of the file to write the output to."
  )

expressionInfo :: ParserInfo (IO ())
expressionInfo = info (helper <*> sheetsOptParser)
  (  fullDesc
  <> header "Expression Details Sheet Creator"
  <> progDesc "Gathers reslut and log .csv files and collects them into a single .xslx file."
  )

sheetsCommands :: Parser (IO ())
sheetsCommands = subparser
  ( command "expression-details" expressionInfo
  )

sheetsInfo :: ParserInfo (IO ())
sheetsInfo = info (helper <*> sheetsCommands)
  ( fullDesc
  <> header "Sheet Creater"
  )
