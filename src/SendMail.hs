{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SendMail
  ( JobMessage
  , sendMessage
  , scheduledMessage
  , stillRunningMessage
  , jobsCompletedMessage
  , nothingScheduledMessage
  , notStartedMessage
  ) where

import ClassyPrelude
import Network.Mail.SMTP
import Data.Time
import Network.Mail.Mime (Mail, Part)
import Types

data JobMessage
  = Scheduled Text
  | NothingScheduled Text
  | Running Text
  | Completed Text
  | NotStarted Text

sendIt :: Mail -> IO ()
-- sendIt = sendMail' "10.102.5.14" 2525
sendIt = sendMail "10.100.2.21"

showJobNames :: [RunName] -> Text
showJobNames = foldl' (\x y -> x <> "&bull; " <> showRunName y <> "<br>") mempty
  where
    showRunName (RunName name) = name

scheduledMessage :: [RunName] -> LocalTime -> JobMessage
scheduledMessage jobNames time = Scheduled $ 
  "The following EPC Post Commit performance tests have been scheduled for execution at " <> tshow time <> ".<br><br>"
  <> showJobNames jobNames

stillRunningMessage :: [RunName] -> LocalTime -> JobMessage
stillRunningMessage jobNames time = Running $ 
  "The following EPC Post Commit performance tests are still scheduled to be run as of " <> tshow time <> ".<br><br>"
  <> showJobNames jobNames

jobsCompletedMessage :: LocalTime -> JobMessage
jobsCompletedMessage time = Completed $ 
  "All EPC Post Commit performance tests for " <> tshow (localDay time) <> " have completed. preprod is now free."

nothingScheduledMessage :: LocalTime -> JobMessage
nothingScheduledMessage time = NothingScheduled $
  "There are no EPC Post Commit performance tests scheduled for the " <> tshow time <> " window."

notStartedMessage :: LocalTime -> JobMessage
notStartedMessage time = NotStarted $
  "As of " <> tshow time <> ", there are jobs scheduled but they have not started yet."

makeMail
  :: LocalTime -> [Part] -> Network.Mail.Mime.Mail
makeMail time = simpleMail (Address Nothing "joshua.mccartney@itgfirm.com")
  [Address Nothing "joshua.mccartney@itgfirm.com"]
  mempty
  mempty
  (tshow (localDay time) <> " Post Commit Test Execution")

sendMessage :: JobMessage -> IO ()
sendMessage txt = do
  ct <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz ct
      message = htmlPart (fromStrict $ fromJobMessage txt) `cons` mempty
  res <- tryAny $ sendIt $ makeMail localTime message
  case res of
    Left exc -> hPutStrLn stderr $ tshow exc
    Right x -> return x

fromJobMessage :: JobMessage -> Text
fromJobMessage (Scheduled txt) = txt
fromJobMessage (Running txt) = txt
fromJobMessage (Completed txt) = txt
fromJobMessage (NothingScheduled txt) = txt
fromJobMessage (NotStarted txt) = txt
