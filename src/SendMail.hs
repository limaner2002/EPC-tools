{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SendMail
  ( JobMessage
  , sendMessage
  , scheduledMessage
  , stillRunningMessage
  , jobsCompletedMessage
  ) where

import ClassyPrelude
import Network.Mail.SMTP
import Data.Time
import Network.Mail.Mime (Mail, Part)

data JobMessage
  = Scheduled Text
  | Running Text
  | Completed Text

sendIt :: Mail -> IO ()
sendIt = sendMail' "10.102.5.61" 2525

showJobNames :: [Text] -> Text
showJobNames = foldl' (\x y -> x <> "&bull; " <> y <> "<br>") mempty

scheduledMessage :: [Text] -> LocalTime -> JobMessage
scheduledMessage jobNames time = Scheduled $ 
  "The following EPC Post Commit performance tests have been scheduled for execution at " <> tshow time <> ".<br><br>"
  <> showJobNames jobNames

stillRunningMessage :: [Text] -> LocalTime -> JobMessage
stillRunningMessage jobNames time = Running $ 
  "The following EPC Post Commit performance tests are still scheduled to be run as of " <> tshow time <> ".<br><br>"
  <> showJobNames jobNames

jobsCompletedMessage :: LocalTime -> JobMessage
jobsCompletedMessage time = Completed $ 
  "All EPC Post Commit performance tests for " <> tshow (localDay time) <> " have completed. preprod is now free."

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
  sendIt $ makeMail localTime message

fromJobMessage :: JobMessage -> Text
fromJobMessage (Scheduled txt) = txt
fromJobMessage (Running txt) = txt
fromJobMessage (Completed txt) = txt
