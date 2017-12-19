{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scripts.CreateCSCase where

import ClassyPrelude
import Control.Lens
import Control.Lens.Action.Reified
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
-- import Control.Monad.Random
import Data.Random
import Control.Monad.Time
import Control.Monad.Except

createCSCase :: (RunClient m, MonadLogger m, MonadTime m, MonadCatch m, MonadThreadId m, MonadRandom m, MonadError ServantError m, MonadDelay m) => AppianT m Text
createCSCase = do
  v <- actionsTab
    >>= (\v -> handleResult $ v ^. getTaskProcId "Create a Customer Service Case")
    >>= landingPageActionEx
    >>= sendUpdates "Title, Description, and Topic" (MonadicFold (to (textUpdate "Title" "PerfTest2"))
                    <|> MonadicFold (to (paragraphUpdate "Description" "A description goes in here."))
                    <|> MonadicFold (to (dropdownUpdate "Topic" 11))
                    )
    >>= sendUpdates "Rest" (MonadicFold (to (dropdownUpdate "Subtopic" 2))
                    <|> MonadicFold (to (dropdownUpdate "Priority" 3))
                    <|> MonadicFold (to (dropdownUpdate "Inquiry Type" 2))
                    <|> MonadicFold (to (textUpdate "First Name" "This is my first name!"))
                    <|> MonadicFold (to (textUpdate "Last Name" "This is my last name!"))
                    <|> MonadicFold (to (textUpdate "Email" "somename@someplace.com"))
                    <|> MonadicFold (to (textUpdate "Phone" "0123456789"))
                    <|> MonadicFold (to (buttonUpdate "Submit"))
                    )
  case v ^? deep (filtered $ has $ key "#v" . _String . suffixed "has been created") . key "#v" . _String of
    Nothing -> fail "The case was not created!"
    Just txt -> return txt

getTaskProcId :: (Contravariant f, Applicative f, Plated s, AsValue s) => Text -> (Result ProcessModelId -> f (Result ProcessModelId)) -> s -> f s
getTaskProcId label = hasKeyValue "displayLabel" label . key "processModelId" . to fromJSON . to (fmap ProcessModelId)

handleResult :: Monad m => Result a -> AppianT m a
handleResult (Error err) = fail err
handleResult (Success a) = pure a

dropdownUpdateWith :: (ToUpdate a, Contravariant f, Applicative f, Plated s, AsValue s, AsJSON s) => (DropdownField -> a) -> Text -> (Update -> f Update) -> s -> f s
dropdownUpdateWith f label = getDropdown label . to f . to toUpdate

dropdownRandom :: MonadRandom m => DropdownField -> m DropdownField
dropdownRandom df = do
  let n = df ^. dfChoices . to length
  idx <- sample $ uniform 1 (n+1)
  return $ dfValue .~ idx $ df
