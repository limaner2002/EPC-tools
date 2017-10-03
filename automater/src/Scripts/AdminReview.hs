{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.AdminReview where

import ClassyPrelude
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Reified
import Scripts.Common
import Scripts.ReviewCommon
import Scripts.Assignment
import Scripts.InitialReview
import Servant.Client

adminReview :: ClientEnv -> ReviewUsers -> IO (Either SomeException Value)
adminReview env revUsers = do
  let reviewManager = userReviewerManager revUsers
      initialReviewer = userInitialReviewer revUsers
      finalReviewer = userFinalReviewer revUsers
      solixReviewer = userSolixReviewer revUsers
      usacReviewer = userUsacReviewer revUsers
      runIt act login = concurrently (loggingFunc "blah!") (tryAny (runAppian act env login) >>= \r -> (atomically $ writeTChan logChan Appian.Client.Done) >> return r)
      runItRetry action login = retryIt (login ^! act (runIt action) . _2 . to join)
  -- runItRetry (assignment adminInitial2017 (initialReviewer ^. username . to AppianUsername)) reviewManager
  --   >>= mapErr (runItRetry (withReviewConf $ flip manageAppealDetails adminInitial2017) initialReviewer)
  --   >>= mapErr (runItRetry (withReviewConf $ flip initialReview adminInitial2017) initialReviewer)
    -- >>= mapErr (runItRetry (assignment adminFinal2017 (finalReviewer ^. username . to AppianUsername)) reviewManager)
    -- >>= mapErr (runItRetry (withReviewConf $ finalReview adminFinal2017) finalReviewer)
    -- >>= mapErr (runItRetry (assignment adminSolix2017 (solixReviewer ^. username . to AppianUsername)) reviewManager)
    -- >>= mapErr (runItRetry (withReviewConf $ finalReview adminSolix2017) solixReviewer)
  runItRetry (assignment adminUsac2017 (usacReviewer ^. username . to AppianUsername)) reviewManager
    >>= mapErr (runItRetry (withReviewConf $ finalReview adminUsac2017) usacReviewer)

mapErr :: Monad m => m (Either SomeException b) -> Either SomeException a -> m (Either SomeException b)
mapErr _ (Left exc) = return $ Left exc
mapErr act (Right _) = act

withReviewConf :: MonadIO m => (ReviewConf -> m a) -> m a
withReviewConf f = do
  conf <- newReviewConf
  f conf

data ReviewUsers = ReviewUsers
  { userReviewerManager :: Login
  , userInitialReviewer :: Login
  , userFinalReviewer :: Login
  , userSolixReviewer :: Login
  , userUsacReviewer :: Login
  } deriving Show

testReviewUsers :: ReviewUsers
testReviewUsers = ReviewUsers
  (Login "bwuser@mailinator.com" "USACuser123$")
  (Login "initial_mok_reviewer@testmail.usac.org" "Usac123$")
  (Login "final_mok_reviewer@testmail.usac.org" "Usac123$")
  (Login "solixqareviewer@mailinator.com" "USACuser123$")
  (Login "usacqareviewer@mailinator.com" "qweR123!")

preprodReviewUsers :: ReviewUsers
preprodReviewUsers = ReviewUsers
  (Login "christine.forsythe@sl.universalservice.org" "EPCPassword123!")
  (Login "jennifer.gawronski@sl.universalservice.org" "EPCPassword123!")
  (Login "karen.hulmes@sl.universalservice.org" "EPCPassword123!")
  (Login "brian.kickey@solixinc.com" "EPCPassword123!")
  (Login "carolyn.mccornac@usac.org" "EPCPassword123!")
