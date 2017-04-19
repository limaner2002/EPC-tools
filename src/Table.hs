{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Table
  ( tableInfo
  ) where

import ClassyPrelude
import Lucid
import MachineUtils
import ParseCSV
import qualified Control.Arrow.Machine.Misc.Discrete as D
import Options.Applicative as OA hiding (header)
import Options.Applicative.Types

header :: Html ()
header = head_ $ link_ [rel_ "stylesheet", href_ "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"]

renderTable :: ToHtml a => [a] -> [[a]] -> Html ()
renderTable head body =
  table_ [class_ "pure-table"] $ do
    tableHead head
    tableBody body

tableHead :: ToHtml a => [a] -> Html ()
tableHead head = thead_ $ mapM_ (th_ . toHtml) head

tableBody :: ToHtml a => [[a]] -> Html ()
tableBody body = tbody_ $ mapM_ (tr_ . renderRow) body
  where
    renderRow = mapM_ (td_ . toHtml)

renderPage :: (ArrowApply a, ToHtml b) => ProcessA a (Event [b]) (Event (Html ()))
renderPage = dSwitch head body
  where
    head = proc _ -> do
      evt <- now -< ()
      h <- evMap (const header) >>> evMap (<> toHtmlRaw ("<body>" :: Text)) >>> evMap asHtml -< evt
      returnA -< (h, h)
    body _ = proc input -> do
      tbl <- renderTableP -< input
      ended <- onEnd -< input
      res <- gather -< [tbl, toHtmlRaw ("</body>" :: Text) <$ ended]
      returnA -< res
    asHtml = id :: Html () -> Html ()

renderTableP :: (ArrowApply a, ToHtml b) => ProcessA a (Event [b]) (Event (Html ()))
renderTableP = dSwitch head body
  where
    head = proc input -> do
      h <- renderHeadP >>> evMap (toHtmlRaw ("<table class=\"pure-table\">" :: Text) <>) -< input
      returnA -< (h, h)
    body _ = proc input -> do
      row <- renderRowP -< input
      ended <- onEnd -< input
      res <- gather -< [row, toHtmlRaw ("</table>" :: Text) <$ ended]
      returnA -< res

renderRowP :: (ArrowApply a, ToHtml b) => ProcessA a (Event [b]) (Event (Html ()))
renderRowP = proc input -> do
  res <- evMap (mapM_ (td_ . toHtml)) >>> evMap tr_ -< input
  returnA -< res

renderHeadP :: (ArrowApply a, ToHtml b) => ProcessA a (Event [b]) (Event (Html ()))
renderHeadP = proc input -> do
  res <- evMap (mapM_ (th_ . toHtml)) >>> evMap thead_ -< input
  returnA -< res

mergeEvents' :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEvents' = proc (evtA, evtB) -> do
  mA <- evMap Just >>> D.hold Nothing -< evtA
  mB <- evMap Just >>> D.hold Nothing -< evtB
  res <- D.arr2 f >>> D.edge >>> filterJust -< (mA, mB)
  returnA -< res
  where
    f mA mB = (,) <$> mA <*> mB

readTable :: FilePath -> CSVSettings -> IO ()
readTable fp settings = runRMachine_ (sourceFile >>> machineParser' (parseRow settings) >>> renderPage >>> evMap renderText >>> machine (putStr . toStrict) >>> onEnd >>> machine (const $ putStrLn "")) [fp]

tableInfo :: ParserInfo (IO ())
tableInfo = info (helper <*> tableParser) fullDesc

tableParser :: Parser (IO ())
tableParser = readTable <$>
  strOption
  (  long "filepath"
  <> short 'p'
  )
  <*> parseCSVSettings

parseCSVSettings :: Parser CSVSettings
parseCSVSettings =
  CSVSettings <$>
        ( option charOpt
          (  long "separator"
            <> short 's'
            <> help "The character to use as the field separator."
          )
          <|> pure ','
        )
        <*>
        ( option charOpt
          (  long "enclosedBy"
            <> short 'e'
            <> help "The character used to enclose a field."
          )
          <|> pure '"'
        )
        <*>
        ( option charOpt
          (  long "newline"
            <> short 'n'
            <> help "The character to use as the newline."
          )
          <|> pure '\n'
        )


charOpt :: ReadM Char
charOpt = do
  str <- readerAsk
  case str of
    [c] -> return c
    _ -> fail "Please provide only 1 character."

