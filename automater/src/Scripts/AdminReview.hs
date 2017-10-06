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

fullReview :: ClientEnv -> ReviewUsers -> FullReviewConf -> IO (Either SomeException Value)
fullReview env revUsers fullReviewConf = do
  let reviewManager = userReviewerManager revUsers
      initialReviewer = userInitialReviewer revUsers
      finalReviewer = userFinalReviewer revUsers
      solixReviewer = userSolixReviewer revUsers
      usacReviewer = userUsacReviewer revUsers
      runIt act login = concurrently (loggingFunc "blah!") (tryAny (runAppian act env login) >>= \r -> (atomically $ writeTChan logChan Appian.Client.Done) >> return r)
      runItRetry action login = retryIt (login ^! act (runIt action) . _2 . to join)
      initial = initialReviewConf fullReviewConf
      final = finalReviewConf fullReviewConf
      solix = solixReviewConf fullReviewConf
      usac = usacReviewConf fullReviewConf
      manageAppealDeets = case reviewType initial of
                            RevAdminCorrection -> runItRetry (withReviewConf $ flip manageAppealDetails initial) initialReviewer
                            _ -> return $ Right Null

      maybeFinal res = case reviewType final of
                         RevForm486 -> return res
                         _ -> mapErr (runItRetry (assignment final (finalReviewer ^. username . to AppianUsername)) $ trace "Final Assign" reviewManager) res
                                >>= mapErr (runItRetry (withReviewConf $ finalReview final) $ trace "Final Review" finalReviewer)


  runItRetry (assignment initial (initialReviewer ^. username . to AppianUsername)) (trace "Initial Assign" reviewManager)
    >>= mapErr manageAppealDeets
    >>= mapErr (runItRetry (withReviewConf $ flip initialReview initial) $ trace "Initial Review" initialReviewer)
    >>= maybeFinal
    >>= mapErr (runItRetry (assignment solix (solixReviewer ^. username . to AppianUsername)) (trace "Solix Assign" reviewManager))
    >>= mapErr (runItRetry (withReviewConf $ finalReview solix) $ trace "Solix Review" solixReviewer)
    >>= mapErr (runItRetry (assignment usac (usacReviewer ^. username . to AppianUsername)) (trace "Usac Assign" reviewManager))
    >>= mapErr (runItRetry (withReviewConf $ finalReview usac) $ trace "Usac Review" usacReviewer)

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

data FullReviewConf = FullReviewConf
  { initialReviewConf :: ReviewBaseConf
  , finalReviewConf :: ReviewBaseConf
  , solixReviewConf :: ReviewBaseConf
  , usacReviewConf :: ReviewBaseConf
  } deriving Show

testReviewUsers :: ReviewUsers
testReviewUsers = ReviewUsers
  (Login "bwuser@mailinator.com" "USACuser123$")
  (Login "initial_mok_reviewer@testmail.usac.org" "Usac123$")
  (Login "final_mok_reviewer@testmail.usac.org" "Usac123$")
  (Login "solixqareviewer@mailinator.com" "USACuser123$")
  (Login "usacqareviewer@mailinator.com" "qweR123!")

preprodAdminReviewUsers :: ReviewUsers
preprodAdminReviewUsers = ReviewUsers
  (Login "christine.forsythe@sl.universalservice.org" "EPCPassword123!")
  (Login "jennifer.gawronski@sl.universalservice.org" "EPCPassword123!")
  (Login "karen.hulmes@sl.universalservice.org" "EPCPassword123!")
  (Login "brian.kickey@solixinc.com" "EPCPassword123!")
  (Login "carolyn.mccornac@usac.org" "EPCPassword123!")

adminReview env revUsers = fullReview env revUsers adminReviewConf

adminReviewConf = FullReviewConf adminInitial2017 adminFinal2017 adminSolix2017 adminUsac2017

preprodSpinReviewUsers :: ReviewUsers
preprodSpinReviewUsers = ReviewUsers
  (Login "lorraine.lipka@sl.universalservice.org" "EPCPassword123!")
  (Login "kevin.conroy@sl.universalservice.org" "EPCPassword123!")
  (Login "victor.guevara@sl.universalservice.org" "EPCPassword123!")
  (Login "brian.kickey@solixinc.com" "EPCPassword123!")
  (Login "cjames@usac.org" "EPCPassword123!")

spinReview env revUsers = fullReview env revUsers spinReviewConf

spinReviewConf = FullReviewConf spinInitial2017 spinFinal2017 spinSolix2017 spinUsac2017

preprodForm486Users :: ReviewUsers
preprodForm486Users = ReviewUsers
  (Login "krobinson@usac.org" "EPCPassword123!")
  (Login "gregory.disarno@solixinc.com" "EPCPassword123!")
  (Login "mark.goldberg@sl.universalservice.org" "EPCPassword123!")
  (Login "robin.reingold@sl.universalservice.org" "EPCPassword123!")
  (Login "avery.scott@sl.universalservice.org" "EPCPassword123!")

form486Review env revUsers = fullReview env revUsers form486ReviewConf

form486ReviewConf = FullReviewConf form486Initial2017 form486Final2017 form486Solix2017 form486Usac2017 

preprodServSubUsers :: ReviewUsers
preprodServSubUsers = ReviewUsers
  (Login "rae.coan@sl.universalservice.org" "EPCPassword123!")
  (Login "sgabaly@solixinc.com" "EPCPassword123!")
  (Login "brian.oconnell@sl.universalservice.org" "EPCPassword123!")
  (Login "brian.kickey@solixinc.com" "EPCPassword123!")
  (Login "jwalsh@usac.org" "EPCPassword123!")

servSubReview env revUsers = fullReview env revUsers servSubConf

servSubConf = FullReviewConf servSubInitial2017 servSubFinal2017 servSubSolix2017 servSubUsac2017
