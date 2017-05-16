{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler.Lib
    ( someFunc
    , jobTable
    , createJob
    , header
    , runJobsButton
    , scheduleInput
    , pureButton
    ) where

import Lucid
import ClassyPrelude hiding (for_)
import Control.Arrow
import Scheduler.Types
import Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

jobTable :: JobQueue a -> Html ()
jobTable jobs = do
  table_ [class_ "pure-table"] $ do
    thead_ $ tr_ $ do
      th_ "#"
      th_ "Job Name"
      th_ "Status"
      th_ mempty
    tbody_ $ mapM_ (tr_ . dispJob) $ zip [1..] (jobs ^. qJobs)
  addJobButton
 where
   dispJob (n, job) = td_ (toHtml $ tshow n) <> (td_ $ toHtml $ job ^. jobName) <> (td_ $ toHtml $ tshow $ job ^. jobStatus) <> (td_ $ modLinks n)
   modLinks :: Int -> Html ()
   modLinks n = do
     a_ [href_ ("/remJob?idx=" <> tshow (n-1))] $ img_ [src_ "icon/remove"]
     -- a_ [href_ ("/moveUp?idx=" <> tshow (n-1))] $ img_ [src_ "icon/arrUp"]
     -- a_ [href_ ("/moveDown?idx=" <> tshow (n-1))] $ img_ [src_ "icon/arrDown"]

createJob :: Html ()
createJob = form_ [class_ "pure-form pure-form-stacked", action_ "/addJob", method_ "post", enctype_ "application/json"] $ fieldset_ $ do
  legend_ "Add a job to the queue"
  label_ [for_ "job-name"] "Job Name"
  input_ [name_ "job-name", type_ "text", class_ "pure-u-23-24"]
  label_ [for_ "job-val"] "Config File Path"
  input_ [name_ "job-val", type_ "text", class_ "pure-u-23-24"]
  button_ [type_ "submit", class_ "pure-button pure-button-primary"] "Add"

header :: Html ()
header = head_ $ link_ [rel_ "stylesheet", href_ "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"]

addJobButton :: Html ()
addJobButton = a_ [class_ "pure-button pure-button-primary", href_ "/new"] "Add Job"

runJobsButton :: Html ()
runJobsButton = a_ [class_ "pure-button", href_ "/runJobs"] "Run Jobs Now"

dryRunButton :: Html ()
dryRunButton = a_ [class_ "pure-button", href_ "/dryRun"] "Dry Run"

scheduleInput :: Html ()
scheduleInput = do
  form_ [class_ "pure-form pure-form-stacked", action_ "/schedule", method_ "post", enctype_ "application/json"] $
    fieldset_ $ do
      label_ [for_ "when"] "Start jobs at:"
      input_ [id_ "when", name_ "date", type_ "date"]
      input_ [id_ "when", name_ "time", type_ "time"]

      button_ [type_ "submit", class_ "pure-button pure-button-primary"] "Schedule"

pureButton :: Term [Attribute] result => Text -> result
pureButton ref = a_ [class_ "pure-button pure-button-error", href_ ref]
