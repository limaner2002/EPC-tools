{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SendMail
  ( JobMessage
  , sendMessage
  , scheduledMessage
  , stillRunningMessage
  , jobsCompletedMessage
  , nothingScheduledMessage
  , notStartedMessage
  , showBatchPlan
  ) where

import ClassyPrelude hiding ((<>))
import Data.Monoid ((<>))
import Network.Mail.SMTP
import Data.Time
import Network.Mail.Mime (Mail, Part)
import Types
import qualified Lucid as L
import Lucid (Html, renderText)
import qualified Data.Text.Lazy as TL

data JobMessage
  = Scheduled (Html ())
  | NothingScheduled (Html ())
  | Running (Html ())
  | Completed (Html ())
  | NotStarted (Html ())

sendIt :: Mail -> IO ()
-- sendIt = sendMail' "10.102.5.14" 2525
sendIt = sendMail "10.100.2.21"

showBatchPlan :: BatchOpts Validated -> Html ()
showBatchPlan batchOpts = -- foldl' (\x y -> x <> "&bull; " <> showRunName (runName y) <> "<br>" <> showJobDelay (sleepTime y)) mempty opts
  L.ul_ $ foldl' (\x y -> x <> renderItem y) mempty opts
  where
    opts = fromBatchOpts batchOpts
    renderItem item = L.li_ $ do
      L.toHtml $ showRunName $ runName item
      L.ul_ $ do
        L.li_ $ L.toHtml $ showJobDelay (sleepTime item)
        L.li_ $ L.toHtml $ showRuns (nRuns item)
        L.li_ $ L.toHtml $ showUsers (nUsers item)
    showRuns (Run n) = tshow n <> " runs"
    showUsers l = "For " <> intercalate ", " (lead l) <> ", and " <> showNUsers (lastEx l) <> " users."
    lead = fmap showNUsers . reverse . drop 1 . reverse
    showNUsers (NUsers n) = tshow n

scheduledMessage :: BatchOpts Validated -> ScheduledTime -> JobMessage
scheduledMessage jobNames time = Scheduled $
  L.toHtml ("The following EPC Post Commit performance tests have been scheduled for execution at " <> tshow (fromScheduledTime time))
  <> showBatchPlan jobNames

stillRunningMessage :: BatchOpts Validated -> LocalTime -> JobMessage
stillRunningMessage jobNames time = Running $
  L.toHtml ("The following EPC Post Commit performance tests are still scheduled to be run as of " <> tshow time <> ".")
  <> showBatchPlan jobNames

jobsCompletedMessage :: LocalTime -> JobMessage
jobsCompletedMessage time = Completed $ L.toHtml $
  "All EPC Post Commit performance tests for " <> tshow (localDay time) <> " have completed. preprod is now free."

nothingScheduledMessage :: LocalTime -> JobMessage
nothingScheduledMessage time = NothingScheduled $ L.toHtml $ 
  "There are no EPC Post Commit performance tests scheduled for the " <> tshow time <> " window."

notStartedMessage :: LocalTime -> JobMessage
notStartedMessage time = NotStarted $ L.toHtml $ 
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
      message = htmlPart (fromJobMessage txt) `cons` mempty
  res <- tryAny $ sendIt $ makeMail localTime message
  case res of
    Left exc -> hSay stderr $ tshow exc
    Right x -> return x

fromJobMessage :: JobMessage -> TL.Text
fromJobMessage (Scheduled txt) = renderText txt
fromJobMessage (Running txt) = renderText txt
fromJobMessage (Completed txt) = renderText txt
fromJobMessage (NothingScheduled txt) = renderText txt
fromJobMessage (NotStarted txt) = renderText txt
