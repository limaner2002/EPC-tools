{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Validate
  ( validateBatchOpts
  , Types.BatchOpts
  ) where

import ClassyPrelude
import Text.XML.HXT.Core
import Control.Monad.State
import Types

data ValidationResult = ValidationResult
  { valMsgs :: [(RunName, String)]
  , valErrors :: Bool
  } deriving Show

instance Monoid ValidationResult where
  mempty = ValidationResult mempty False
  (ValidationResult msgA errA) `mappend` (ValidationResult msgB errB) = ValidationResult (msgA <> msgB) (errA || errB)

data ThreadGroup = ThreadGroup
  { nThreads :: String
  , nLoops :: String
  } deriving Show

data Validation = Validation
  { propName :: String
  , propValue :: String
  , propValRes :: Bool
  }

showValidation :: Validation -> String
showValidation (Validation pn pv pvr) =
  pn <> ": " <> pv <> "\t" <> show pvr

newtype Validations = Validations [Validation]

newtype AggFilename = AggFilename String
  deriving Show

instance Monoid Validations where
  mempty = Validations mempty
  (Validations l1) `mappend` (Validations l2) = Validations (l1 `mappend` l2)

instance Semigroup Validations

showValidations :: Validations -> String
showValidations (Validations l) = str <> "\n"
  where
    str = intercalate "\n" $ fmap showValidation l

getThreadGroup :: ArrowXml a => a XmlTree ThreadGroup
getThreadGroup = proc input -> do
  tg <- hasName "ThreadGroup" -< input
  loops <- deep (hasAttrValue "name" (isSuffixOf ".loops")) /> getText -< tg
  threads <- getChildren >>> hasAttrValue "name" (isSuffixOf ".num_threads") /> getText -< tg
  returnA -< ThreadGroup threads loops

threadGroupGold :: ThreadGroup
threadGroupGold = ThreadGroup "${users}" "${loopCount}"

checkThreadGroup :: Arrow a => a ThreadGroup Validations
checkThreadGroup = arr (checkThreadGroup_ threadGroupGold)

checkThreadGroup_ :: ThreadGroup -> ThreadGroup -> Validations
checkThreadGroup_ (ThreadGroup threadsA loopsA) (ThreadGroup threadsB loopsB) =
  Validations [ Validation "threadgroup.threads" threadsB (threadsA == threadsB)
              , Validation "threadgroup.loops" loopsB (loopsA == loopsB)
              ]

getAggFilename :: ArrowXml a => a XmlTree AggFilename
getAggFilename = hasName "ResultCollector" /> hasAttrValue "name" (=="filename") /> getText >>> arr AggFilename

filenameGold :: AggFilename
filenameGold = AggFilename "${outputFile}"

checkAggFilename :: Arrow a => a AggFilename Validations
checkAggFilename = arr (checkAggFilename_ filenameGold)

checkAggFilename_ :: AggFilename -> AggFilename -> Validations
checkAggFilename_ (AggFilename a) (AggFilename b) =
  Validations [ Validation "aggregateReport.outputFile" b (a == b)]

checkProperties :: Arrow a => a (ThreadGroup, AggFilename) Validations
checkProperties = checkThreadGroup *** checkAggFilename >>> arr combine
  where
    combine (a, b) = a <> b

getProperties :: ArrowXml a => a XmlTree (ThreadGroup, AggFilename)
getProperties = deep getThreadGroup &&& deep getAggFilename

checkScript :: MonadIO m => FilePath -> m String
checkScript fp = liftIO $ do
  ctnt <- readFile fp
  r <- runX $ readString [withValidate no, withWarnings no] ctnt >>> getProperties >>> checkProperties
  return $ foldl' (<>) mempty $ fmap showValidations r

validateBatchOpts :: (MonadThrow m, MonadState ValidationResult m, MonadIO m)
  => BatchOpts UnValidated -> m (BatchOpts Validated)
validateBatchOpts (BatchOpts opts) = do
  res <- runKleisli (checkBatchOpts >>> checkScripts) opts
  return $ BatchOpts res

checkScripts :: (MonadState ValidationResult m, MonadIO m) => Kleisli m [JMeterOpts] [JMeterOpts]
checkScripts = (arr (fmap getIt) >>> Kleisli (mapM checkScript') >>> mkValidation >>> Kleisli addCheck) &&& id >>> arr snd
  where
    getIt opt = (runName opt, jmxPath opt)
    checkScript' (rn, fp) = do
      res <- checkScript fp
      return (rn, res)
    mkValidation = arr $ \v -> ValidationResult v False

checkBatchOpts :: (MonadState ValidationResult m) => Kleisli m [JMeterOpts] [JMeterOpts]
checkBatchOpts = Kleisli checkIt
  where
    checkRunNames = all (==1) . countRunNames
    countRunNames opts = foldl' insertName (mempty :: Map RunName Int) opts
    insertName mp v = alterMap countName (runName v) mp
    countName Nothing = Just 1
    countName (Just n) = Just (n + 1)
    checkIt opts = case checkRunNames opts of
      True -> return $ opts
      False -> throwM $ InvalidBatchOpts "Each test should have a unique name."

addCheck :: MonadState ValidationResult m => ValidationResult -> m ()
addCheck new = do
  old <- get
  put (old <> new)
