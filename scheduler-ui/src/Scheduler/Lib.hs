{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler.Lib
    ( someFunc
    , jobTable
    , createJob
    , header
    , runJobsButton
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
    tbody_ $ mapM_ (tr_ . dispJob) $ zip [1..] jobs
  addJobButton
 where
   dispJob (n, job) = td_ (toHtml $ tshow n) <> (td_ $ toHtml $ job ^. jobName) <> (td_ $ toHtml $ tshow $ job ^. jobStatus)

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
