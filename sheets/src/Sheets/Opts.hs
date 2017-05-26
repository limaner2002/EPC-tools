{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sheets.Opts
  ( -- sheetsInfo
  ) where

import ClassyPrelude
-- import qualified Sheets.ExpressionDetailsSheet as Exp
import Options.Applicative
-- import qualified Sheets.AggregateSheets as Agg

-- sheetsOptParser :: Parser (IO ())
-- sheetsOptParser = Exp.createSheet
--   <$> strOption
--   (  long "sourceDir"
--   <> short 's'
--   <> metavar "SOURCE_DIR"
--   <> help "The directory which holds the log files to read"
--   )
--   <*> strOption
--   (  long "outputFile"
--   <> short 'o'
--   <> metavar "OUTPUT_FILE"
--   <> help "The path of the file to write the output to."
--   )

-- expressionInfo :: ParserInfo (IO ())
-- expressionInfo = info (helper <*> sheetsOptParser)
--   (  fullDesc
--   <> header "Expression Details Sheet Creator"
--   <> progDesc "Gathers reslut and log .csv files and collects them into a single .xslx file."
--   )

-- aggregateOptParser :: Parser (IO ())
-- aggregateOptParser = Agg.createSheet
--   <$> option auto
--   (  long "max-row"
--   <> short 'm'
--   <> metavar "MAX_ROW"
--   <> help "The number of the last row to include in the error formatting of the resulting sheet."
--   )
--   <*> strOption
--   (  long "outFile"
--   <> short 'o'
--   <> metavar "OUT_FILE"
--   <> help "The file to write the resulting .xlss to."
--   )

-- aggregateInfo :: ParserInfo (IO ())
-- aggregateInfo = info (helper <*> aggregateOptParser)
--   (  fullDesc
--   <> header "Aggregate Report Sheet Creator"
--   <> progDesc "Creates a sheet from the aggregate reports."
--   )

-- sheetsCommands :: Parser (IO ())
-- sheetsCommands = subparser
--   (  command "expression-details" expressionInfo
--   <> command "aggregate-sheet" aggregateInfo
--   )

-- sheetsInfo :: ParserInfo (IO ())
-- sheetsInfo = info (helper <*> sheetsCommands)
--   ( fullDesc
--   <> header "Sheet Creater"
--   )
