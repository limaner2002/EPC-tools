{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module JBossLog
  ( jbossInfo
  ) where

import ClassyPrelude hiding (filter)
import MachineUtils
import ParseCSV
import Data.Time
import Common
import Control.Monad.State
import Control.Monad.Logger
import Data.Default

import Options.Applicative
import Options.Applicative.Types

data JBossLog = JBossLog
  { timeStamp :: TimeOfDay
  , message :: [Text]
  } deriving Show

type Row a = [a]

instance FromRow [] Text JBossLog where
  fromRow (ts:msg) = JBossLog <$> readTS ts <*> pure msg
  fromRow row = throwM $ InvalidCSVRow $ "Could not read row " <> tshow row

data JBossTS = JBossTS Day JBossLog
  deriving Show

instance ToUTCTime JBossTS where
  toUTCTime (JBossTS day l) = UTCTime day (timeOfDayToTime $ timeStamp l)

showJBossLog :: JBossLog -> Text
showJBossLog (JBossLog ts msg) = tshow ts <> "\t" <> foldl' (<>) mempty msg <> "\n"

showJBossTS :: JBossTS -> Text
showJBossTS (JBossTS _ l) = showJBossLog l

readTS :: Monad m => Text -> m TimeOfDay
readTS = parseTimeM True defaultTimeLocale "%X" . unpack

processLog :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m) => FilePath -> JMeterTimeStamp -> JMeterTimeStamp -> Day -> FilePath -> m ()
processLog sourceFp start end day destFp = do
  logDebugN $ "reading " <> pack sourceFp <> " between " <> tshow start <> " and " <> tshow end <> " on " <> tshow day <> " and saving to " <> pack destFp
  runStateT
    ( runRMachine_
      ( sourceFile
        >>> machineParser' (parseRow def)
        >>> machine fromRow
        -- >>> machine asJBossLog
        >>> evMap toJBossTS
        >>> filter (Kleisli $ filterTime_ 300 start end)
        >>> evMap showJBossTS
        >>> sinkFile destFp
      ) [sourceFp]
    ) Init >>= pure . fst
 where
   toJBossTS jl = JBossTS day jl

jbossParser :: (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => Parser (m ())
jbossParser = processLog
  <$> strOption
    (  long "source"
    <> short 's'
    <> metavar "DESTINATION"
    <> help "The JBoss log to filter."
    )
  <*> option parseJMeterTimeStamp
    (  long "start-time"
    <> metavar "START_TIME"
    <> help "The time at which the test started."
    )
  <*> option parseJMeterTimeStamp
    (  long "end-time"
    <> metavar "END_TIME"
    <> help "The time at which the test finished."
    )
  <*> option parseDay
    (  long "day"
    <> metavar "DAY"
    <> help "The JBoss logs do not include the day in the timestamp, but the JMeter timestamps do have day information. This option is used to add the day to the JBoss timestamp."
    )
  <*> strOption
    (  long "dest"
    <> short 'd'
    <> metavar "DESTINATION"
    <> help "The file path to put the filtered results."
    )

jbossInfo :: (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => ParserInfo (m ())
jbossInfo = info (helper <*> jbossParser)
  (  fullDesc
  <> header "JBoss Log Parser"
  <> progDesc "This is used to extract the relevant sections of the jboss-stdout logfiles from Appian."
  )

parseDay :: ReadM Day
parseDay = readerAsk >>= parseTimeM True defaultTimeLocale "%F"
