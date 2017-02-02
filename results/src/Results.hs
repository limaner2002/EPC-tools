{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Results
  ( resultsInfo
  ) where

import ClassyPrelude hiding (filter)
import Control.Monad.Trans.Resource hiding (throwM)
import Data.Time.Clock.POSIX
import Data.Void (Void)
import Data.Time
import Control.Arrow
import MachineUtils
import ParseCSV
import Data.Default
import Control.Monad.State hiding (mapM_)
import System.FilePath.Glob
import System.FilePath
import Formatting
import qualified Data.Text.Lazy as TL
import Data.Char (isDigit)
import Options.Applicative hiding ((<>))
import qualified Options.Applicative as OA
import Options.Applicative.Types

data JMeterReading = JMeterReading
  { readTimeStamp :: !JMeterTimeStamp
  , readElapsed :: !Int
  , readLabel :: !Text
  , readResponseCode :: !JMeterResponseCode
  , readResponseMsg :: !Text
  , readThreadName :: !Text
  , readDataType :: !Text
  , readSuccess :: !Bool
  , readFailureMessage :: !Text
  , readBytes :: !Int
  , readSentBytes :: !Int
  , readGrpThreads :: !Text
  , readAllThreads :: !Int
  , readLatency :: !Int
  , readIdleTime :: !Int
  , readConnect :: !Int
  } | ReadingHeader
  { rheadTimeStamp :: !Text
  , rheadElapsed :: !Text
  , rheadLabel :: !Text
  , rheadResponseCode :: !Text
  , rheadResponseMsg :: !Text
  , rheadThreadName :: !Text
  , rheadDataType :: !Text
  , rheadSuccess :: !Text
  , rheadFailureMessage :: !Text
  , rheadBytes :: !Text
  , rheadSentBytes :: !Text
  , rheadGrpThreads :: !Text
  , rheadAllThreads :: !Text
  , rheadLatency :: !Text
  , rheadIdleTime :: !Text
  , rheadConnect :: !Text
  } deriving Show

data ExpressionDetails = ExpressionDetails
  { exprTimeStamp :: UTCTime
  , exprName :: Text
  , exprType :: Text
  , exprTotalCount :: Int
  , exprMeanTotalTimems :: Double
  , exprMinTotalTimems :: Int
  , exprMaxTotalTimems :: Int
  } deriving Show

data System = System
  { sysTimeStamp :: UTCTime
  , sysThreadCount :: Int
  , sysDaemonThreadCount :: Int
  , sysUsedHeapSpace :: Int
  , sysAvailableHeapSpace :: Int
  , sysTenuredGenerationUsageAfterCollection :: Int
  , sysTenuredGenerationUsed :: Int
  , sysTenuredGenerationAvailable :: Int
  , sysPermanentGenerationUsed :: Int
  , sysPermanentGenerationAvailable :: Int
  , sysYoungCollectionCount :: Int
  , sysYoungCollectionTime :: Int
  , sysTenuredCollectionCount :: Int
  , sysDiskPartitionUsed :: Int
  , sysDiskPartitionAvailable :: Int
  , sysLoadAverage :: Int
  , sysCPUCoreCount :: Int
  , sysTotalSystemMemory :: Int
  , sysUsedSystemMemory :: Int
  , sysTotalSwapSpace :: Int
  , sysUsedSwapSpace :: Int
  , sysSessionCount :: Int
  }

showExpressionDetails :: ExpressionDetails -> Text
showExpressionDetails (ExpressionDetails ts name typ total mean min max)
  = tshow ts <> "\t" <> name <> "\t" <> typ <> "\t" <> tshow total <> "\t" <> tshow mean <> "\t" <> tshow min <> "\t" <> tshow max

newtype InvalidCSVEntry = InvalidCSVEntry Text
  deriving Show

instance Exception InvalidCSVEntry

newtype InvalidCSVRow = InvalidCSVRow Text
  deriving Show

instance Exception InvalidCSVRow

newtype JMeterTimeStamp = JMeterTimeStamp UTCTime
  deriving Show

data AggregateAccum = AggregateAccum
  { aggNObservations :: !Int
  , aggTotalElapsed :: !Int
  , aggNAboveSLA :: !Int
  }
  deriving (Show, Eq)

data AggregateTransaction = AggregateTransaction
  { transObserved :: !Int
  , transAverage :: !Double
  , transNAboveSLA :: !Int
  , transPctAboveSLA :: !Double
  } deriving (Show, Eq)

mkAggregateTransaction :: AggregateAccum -> AggregateTransaction
mkAggregateTransaction (AggregateAccum n elapsed nAboveSLA) =
  AggregateTransaction n avg nAboveSLA pct
  where
    avg = fromIntegral elapsed / fromIntegral n
    pct = fromIntegral nAboveSLA / fromIntegral n

data AggregateReport = AggregateReport
  { repPctAboveSLA :: !Double
  , repNAboveSLA :: !Int
  , repNObserved :: !Int
  , repHighestAvg :: !Double
  , repLowestAvg :: !Double
  } deriving (Show, Eq)

newtype Percentage = Percentage Double
  deriving (Eq, Ord, Show, Read)

showAggregateReport :: AggregateReport -> TL.Text
showAggregateReport (AggregateReport pct nAbove observed highest lowest)
  = format ( "% of Recorded Response Times greater than 5 seconds: " % fixed 2 % "%  (" % int % "/" % int % ")\n"
           % "Highest Average Response Time: " % fixed 2 % " seconds\n"
           % "Lowest Average Response Time: " % fixed 2 % " seconds\n"
           ) pct nAbove observed (highest / 1000) (lowest / 1000)


instance Default AggregateReport where
  def = AggregateReport 0 0 0 0 (0/0)

mkAggregateReport :: Map Text AggregateTransaction -> AggregateReport
mkAggregateReport transactions = getPctg $ foldl' accumReport def transactions
  where
    accumReport (AggregateReport _ n1 observed1 highest lowest) (AggregateTransaction observed2 avg n2 _) =
      AggregateReport 0 (n1 + n2) (observed1 + observed2) (max highest avg) (min lowest avg)
    getPctg rep@(AggregateReport {repNAboveSLA=nAbove, repNObserved=observed}) = rep {repPctAboveSLA = pctg nAbove observed}
    pctg above observed = (fromIntegral above / fromIntegral observed) * 100

data JMeterResponseCode
  = HTTPResponseCode Int
  | NonHTTPResponseCode Text
  deriving Show

readJMeterResponseCode :: Text -> JMeterResponseCode
readJMeterResponseCode txt = case HTTPResponseCode <$> readMay txt of
  Just resp -> resp
  Nothing -> NonHTTPResponseCode txt

type ReadMap = Map Text AggregateAccum

type Row a = [a]

toReading :: (ArrowApply a, MonadThrow m) => ProcessA a (Event (m [Text])) (Event (m JMeterReading))
toReading = dSwitch before after
  where
    before = proc input -> do
      header <- evMap (join . fmap fromHeader) -< input
      returnA -< (header, header)
    after _ = evMap (join . fmap fromRow)

fromHeader :: MonadThrow m => Row Text -> m JMeterReading
fromHeader [ts, elpsd, label, respCode, respMsg, threadName, dataType, success, failureMessage, bytes, sentBytes, grpThreads, allThreads, latency, idleTime, connect]
  = return $ ReadingHeader ts elpsd label respCode respMsg threadName dataType success failureMessage bytes sentBytes grpThreads allThreads latency idleTime connect
fromHeader row = throwM $ InvalidCSVRow $ "Could not read row " <> tshow row

fromRow :: MonadThrow m => Row Text -> m JMeterReading
fromRow [ts, elpsd, label, respCode, respMsg, threadName, dataType, success, failureMessage, bytes, sentBytes, grpThreads, allThreads, latency, idleTime, connect]
  = JMeterReading
  <$> readJMeterTimeStamp ts
  <*> readThrow elpsd
  <*> pure label
  <*> pure (readJMeterResponseCode respCode) -- readThrow respCode
  <*> pure respMsg
  <*> pure threadName
  <*> pure dataType
  <*> readLowerBool success
  <*> pure failureMessage
  <*> readThrow bytes
  <*> readThrow bytes
  <*> pure grpThreads
  <*> readThrow allThreads
  <*> readThrow latency
  <*> readThrow idleTime
  <*> readThrow connect
fromRow row = throwM $ InvalidCSVRow $ "Could not read row " <> tshow row

readJMeterTimeStamp ts = JMeterTimeStamp . posixSecondsToUTCTime <$> (\x -> fromIntegral x / 1000) <$> (readThrow ts)

exprDetailsFromRow :: MonadThrow m => Row Text -> m ExpressionDetails
exprDetailsFromRow [ts, name, typ, totalCount, meanTotalTime, minTotalTime, maxTotalTime] =
  ExpressionDetails <$> readTimeStamp ts
                    <*> pure name
                    <*> pure typ
                    <*> readThrow totalCount
                    <*> readThrow meanTotalTime
                    <*> readThrow minTotalTime
                    <*> readThrow maxTotalTime
 where
   readTimeStamp = parseTimeM False defaultTimeLocale "%e %b %Y %T %Z" . unpack
exprDetailsFromRow row = throwM $ InvalidCSVRow $ "Could not read row " <> tshow row

addReading :: JMeterReading -> ReadMap -> ReadMap
addReading ReadingHeader {} readingMap = readingMap
addReading JMeterReading {readLabel = label, readElapsed = elapsed} readingMap = alterMap alterReading label readingMap
  where
    alterReading Nothing = Just (AggregateAccum 1 elapsed aboveSLA)
    alterReading (Just (AggregateAccum n et nAboveSLA)) = Just (AggregateAccum (n+1) (et + elapsed) (aboveSLA + nAboveSLA))
    aboveSLA
      | elapsed > 5000 = 1
      | otherwise = 0

readLowerBool :: MonadThrow m => Text -> m Bool
readLowerBool "false" = return False
readLowerBool "true" = return True
readLowerBool entry = throwM $ InvalidCSVEntry $ "Could not read value " <> entry

readThrow entry =
  case readMay entry of
    Nothing -> throwM $ InvalidCSVEntry $ "Could not read value " <> entry
    Just v -> return v

-- data Options
--   = LogFiles FilePath FilePath JMeterTimeStamp JMeterTimeStamp
--   | Report FilePath
--   | Memory FilePath

parseJMeterTimeStamp :: ReadM JMeterTimeStamp
parseJMeterTimeStamp = do
  input <- readerAsk
  let mTs = JMeterTimeStamp . posixSecondsToUTCTime <$> (\x -> fromIntegral x / 1000) <$> (readMay input)
  case mTs of
    Nothing -> readerError $ show input <> " does not appear to be a valid JMeterTimeStamp."
    Just ts -> return ts

-- optionsParser :: Parser Options
-- optionsParser = logsParser <|> reportParser <|> memoryParser
commandsParser :: Parser (IO ())
commandsParser = subparser
  ( command "parselogs" logsInfo
  OA.<> command "createreport" reportInfo
  OA.<> command "testleak" memoryInfo
  )

logsParser :: Parser (IO ())
logsParser = processLogs
  <$> strOption
    (  long "logPathGlob"
    OA.<> metavar "LOGPATH_GLOB"
    OA.<> help "Glob for the logfiles"
    )
  <*> strOption
    (  long "outputDir"
    OA.<> metavar "OUTPUD_DIR"
    OA.<> help "The directory to put the result files in"
    )
  <*> option parseJMeterTimeStamp
    (  long "start-time"
    OA.<> metavar "START_TIME"
    OA.<> help "The time at which the test started."
    )
  <*> option parseJMeterTimeStamp
    (  long "end-time"
    OA.<> metavar "END_TIME"
    OA.<> help "The time at which the test finished."
    )

logsInfo :: ParserInfo (IO ())
logsInfo = info (helper <*> logsParser)
  (  fullDesc
  OA.<> header "Log Parser"
  OA.<> progDesc "This is used to extract the relevant sections of the expression rules details logfiles from Appian."
  )

reportParser :: Parser (IO ())
reportParser = createReport
  <$> strOption
    (  long "testPath"
    OA.<> metavar "TEST_PATH"
    OA.<> help "The directory holding all of the runs for a given test."
    )

reportInfo :: ParserInfo (IO ())
reportInfo = info (helper <*> reportParser)
  (  fullDesc
  OA.<> header "Report Creator"
  OA.<> progDesc "Use this to create a simple report that contains the maximum/minimum request time and the percentage of requests that were above 3seconds."
  )

memoryParser :: Parser (IO ())
memoryParser = testSpaceLeak
  <$> strOption
    (  long "csvPath"
    OA.<> metavar "CSV_PATH"
    OA.<> help "The path to the csv file to parse."
    )

memoryInfo :: ParserInfo (IO ())
memoryInfo = info (helper <*> memoryParser)
  (  fullDesc
  OA.<> header "Space Leak Test"
  OA.<> progDesc "This is for testing the space leak found during RC2.0 performance analysis."
  )

resultsInfo :: ParserInfo (IO ())
resultsInfo = info (helper <*> commandsParser)
  (  fullDesc
  OA.<> progDesc "This contains some tools to help analyse the results of running performance tests."
  OA.<> header "Performance Result Analysis Tools"
  )

processLogs :: FilePath -> FilePath -> JMeterTimeStamp -> JMeterTimeStamp -> IO ()
processLogs logPathGlob outputDir startTime endTime = do
  inFiles <- namesMatching $ logPathGlob
  let outFiles = fmap (\x -> outputDir </> takeFileName x) inFiles
  evalStateT (
    runRMachine_ (readLogs startTime endTime) (zip inFiles outFiles)
    ) Init

-- dispatch :: Options -> IO ()
-- dispatch (LogFiles p o s e) = processLogs p o s e

createReport :: FilePath -> IO ()
createReport path = runRMachine_ aggregateReport [path]
  where
    aggregateReport = getReadMap
      >>> evMap ((fmap . fmap) mkAggregateTransaction)
      >>> evMap (fmap mkAggregateReport)
      >>> evMap (fmap showAggregateReport)
      >>> machine (mapM_ (putStrLn . toStrict))

testSpaceLeak :: FilePath -> IO ()
testSpaceLeak path = runRMachine_ ( sourceDirectory
                                       >>> sourceDirectory
                                       >>> filter (arr $ isSuffixOf ".csv")
                                       >>> evMap id &&& (
                                              sourceFile
                                          >>> machineParser (parseRow def)
                                          )
                                       >>> switchTest toReading
                                       >>> accumulate
                                       >>> evMap asEither
                                       >>> machine print
                                      ) [path]
  where
    accumulate = proc input -> do
      res <- evMap (fmap addReading) >>> evMap (<*>) >>> accum (pure mempty) -< input
      ended <- onEnd -< res <$ input
      returnA -< res <$ ended

-- main :: IO ()
-- main = execParser optsInfo >>= dispatch

readLogs :: (MonadResource m, MonadState FilterState m) => JMeterTimeStamp -> JMeterTimeStamp -> ProcessA (Kleisli m) (Event (FilePath, FilePath)) (Event ())
readLogs startTime endTime = proc input -> do
  mX <- evMap Just >>> hold Nothing -< input
  case mX of
    Nothing -> returnA -< noEvent
    Just (inFile, outFile) -> do
      eDta <- edge >>> anytime (passthroughK print) >>> processLog startTime endTime >>> evMap ((<> "\n") . showExpressionDetails . unsafeFromEither) -< inFile
      res <- (edge *** evMap id) >>> sinkFile_ -< (outFile, eDta)
      returnA -< res
  where
    processLog startTime endTime =
          sourceFile
      >>> evMap asText
      >>> machineParser (parseRow def)
      >>> dropHeader
      >>> evMap (join . mapM exprDetailsFromRow)
      >>> filter (Kleisli $ filterTime 1800 startTime endTime)
    unsafeFromEither (Right v) = v

isBetween_
  :: JMeterTimeStamp
     -> JMeterTimeStamp -> Either t ExpressionDetails -> Bool
isBetween_ start end (Left _) = False
isBetween_ start end (Right v) = isBetween start end v

isBetween
  :: JMeterTimeStamp -> JMeterTimeStamp -> ExpressionDetails -> Bool
isBetween (JMeterTimeStamp startTime) (JMeterTimeStamp endTime) exprDetails
  = startTime <= time && time <= endTime
  where
    time = exprTimeStamp exprDetails

filterTime
  :: MonadState FilterState m =>
     NominalDiffTime
     -> JMeterTimeStamp
     -> JMeterTimeStamp
     -> Either t ExpressionDetails
     -> m Bool
filterTime logTimeDiff start end (Left _) = return False
filterTime logTimeDiff start end (Right v) = filterTime_ logTimeDiff start end v

                                             -- Stateful filter to get timestamps that occurred after the endtime in the log file
filterTime_ :: MonadState FilterState m => NominalDiffTime -> JMeterTimeStamp -> JMeterTimeStamp -> ExpressionDetails -> m Bool
filterTime_ logTimeDiff start@(JMeterTimeStamp startTime) end@(JMeterTimeStamp endTime) exprDetails = do
  filterState <- get
  case filterState of
    Init -> case isBetween start end exprDetails of
      False -> case endTime < (exprTimeStamp exprDetails) of
        False -> return False
        True -> put (GatherAllLast $ exprTimeStamp exprDetails) >> return True
      True -> put Between >> return True
    Between -> case isBetween start end exprDetails of
      True -> return True
      False -> put (GatherAllLast $ exprTimeStamp exprDetails) >> return True
    GatherAllLast ts ->
      case diffUTCTime (exprTimeStamp exprDetails) ts > logTimeDiff of
        False -> return True
        True -> put Done >> return False
    Done -> return False

asEither :: Either SomeException a -> Either SomeException a
asEither = id

getCurrMax :: (Monad m, Ord a) => ProcessA (Kleisli m) (Event a) (Event a)
getCurrMax = constructT kleisli0 $ loop Nothing
  where
    loop Nothing = do
      x <- await
      yield x
      loop (Just x)
    loop (Just v) = do
      x <- await
      case x > v of
        True -> do
          yield x
          loop (Just x)
        False -> do
          yield v
          loop (Just v)

data FilterState
  = Init
  | Between
  | GatherAllLast UTCTime
  | Done
  deriving Show

isError :: JMeterReading -> Bool
isError ReadingHeader {} = False
isError JMeterReading {readResponseCode = respCode} =
  case respCode of
    NonHTTPResponseCode _ -> True
    HTTPResponseCode c -> c /= 200

getErrors :: Either SomeException JMeterReading -> Bool
getErrors (Left _) = False
getErrors (Right reading) = isError reading

data AverageResp = AverageResp Integer Integer Double

getReadMap :: (MonadResource m, MonadMask m) =>
     ProcessA (Kleisli m) (Event FilePath) (Event (Either SomeException ReadMap))
getReadMap = accumulateIt
  where
    fileList = sourceDirectory >>> sourceDirectory >>> filter (arr $ isSuffixOf ".csv")
    readings = fileList >>> evMap id &&& (sourceFile >>> machineParser (parseRow def)) >>> switchTest toReading
    accumulateIt = proc input -> do
      agg <- readings >>> filter (arr (not . getErrors)) >>> filter (arr removeLoop) >>> evMap (fmap addReading) >>> evMap (<*>) >>> accum (pure mempty) -< input
      ended <- onEnd -< agg <$ input
      returnA -< agg <$ ended
    removeLoop (Left _) = True
    removeLoop (Right (ReadingHeader {})) = True
    removeLoop (Right (JMeterReading {readLabel=label})) = label /= "Loop through FRNs" && label /= "Login / Landing Page"

switchTest :: ArrowApply cat => ProcessA cat (Event b) (Event c) -> ProcessA cat (Event a, Event b) (Event c)
switchTest arr = go
  where
    go = proc (evt, input) -> do
      res <- rSwitch arr -< (input, arr <$ evt)
      returnA -< res

filterFcn :: Either SomeException [Text] -> Bool
filterFcn (Left _) = False
filterFcn (Right (_:_:x:_)) = checkIt $ headMay x
  where
    checkIt Nothing = False
    checkIt (Just c) = not $ isDigit c

onEnd' :: ArrowApply a => ProcessA a (Event b) (Event ())
onEnd' = construct loop
  where
    loop = do
      mX <- (Just <$> await) `catchP` pure Nothing
      case mX of
        (Just x) -> seq x loop
        Nothing -> yield () >> stop
