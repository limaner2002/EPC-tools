{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Stats where

import ClassyPrelude hiding (try)
import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as S
import Data.Attoparsec.ByteString.Streaming
import Control.Monad.Trans.Resource
import Control.Arrow
import Control.Lens
import Text.PrettyPrint.Boxes hiding ((<>), char)
import Data.List (transpose)
import Numeric
import Options.Applicative
import Options.Applicative.Types
import Text.Parsec hiding (label, option, (<|>))
import Text.Parsec.String hiding (Parser)
import qualified Text.Parsec.String as P

import Stats.ParseSample
import StreamParser

data Stat = Stat
  { _statTime :: UTCTime
  , _statTotal :: Int
  , _statErrors :: Int
  } deriving Show

type Dict = Map Label Stat

newStat :: UTCTime -> Stat
newStat ts = Stat ts 0 0

makeLenses ''Stat

collectRuns :: [FilePath] -> IO Dict
collectRuns paths = foldM collectStats mempty paths

collectStats :: Dict -> FilePath -> IO Dict
collectStats init path = do
  res <- BSS.readFile
    >>> BSS.dropWhile (/= toEnum (fromEnum '\n'))
    >>> BSS.drop 1
    >>> parsed (parseRow >=> mapM (pure . decodeUtf8) >=> parseHTTPSample $ defaultCSVSettings)
    >>> S.fold collectStat init id
    >>> runResourceT $ path
  return $ S.fst' res

collectStat :: Dict -> HTTPSample -> Dict
collectStat d sample = d & at (sample ^. label) %~ update
  where
    update Nothing = Just $ newStat (sample ^. timeStamp)
    update (Just stat) = Just $ makeStat stat sample

makeStat :: Stat -> HTTPSample -> Stat
makeStat stat sample = stat & statTotal %~ (+1) & statErrors %~ (isError (sample ^. responseCode))
  where
    isError code n
      | code < 400 = n
      | otherwise = n + 1

dispRuns :: [FilePath] -> IO ()
dispRuns paths = do
  res <- collectRuns paths
  printBox . hsep 2 Text.PrettyPrint.Boxes.left . fmap (vcat Text.PrettyPrint.Boxes.left . fmap text) . transpose $ fmap (uncurry statToRow) $ sortOn (^. _2 . statTime) $ mapToList res

statToRow :: Label -> Stat -> [String]
statToRow statLabel stat = [ unpack (statLabel ^. labelVal)
                           , show (stat ^. statTotal)
                           , show (stat ^. statErrors)
                           , showFFloat (Just 2) pctg mempty
                           ]
  where
    pctg = fromIntegral (stat ^. statErrors) / fromIntegral (stat ^. statTotal) * 100

-- foldSplit :: Textual t => t -> [t]
-- foldSplit

accumIt :: (String, Bool) -> Char -> (String, Bool)
accumIt (str, False) ' ' = (str, False)
accumIt (str, True) ' ' = (' ' : str, False)
accumIt (str, False) '\\' = (str, True)
accumIt (str, _) c = (c : str, False)

-- parserInfo :: ParserInfo (IO ())
-- parserInfo = info (helper <*> parser)
--   (  fullDesc
--   <> progDesc "This is a tool to collect and read stats from JMeter tests."
--   )

-- parser :: Parser (IO ())
-- parser = dispRuns
--   <$> option parseMany
--   (  long "paths"
--   <> short 'p'
--   <> help "A list of aggregate_x_x.csv paths"
--   )

-- main :: IO ()
-- main = join $ execParser parserInfo

parseWord :: P.Parser String
parseWord = manyTill anyChar ((string "\\ " >> fail "blah!") <|> char ' ')
  
parseWords :: P.Parser [String]
parseWords = sepBy parseWord (char ' ')
