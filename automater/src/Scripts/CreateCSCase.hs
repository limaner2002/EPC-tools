{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.CreateCSCase where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Appian
import Appian.Types
import Appian.Instances
import Appian.Lens
import Appian.Client
import Control.Monad.Random

createCSCase :: Appian Text
createCSCase = do
  v <- actionsTab
    >>= (\v -> handleResult $ v ^. getTaskProcId "Create a Customer Service Case")
    >>= landingPageActionEx . PathPiece
    >>= sendUpdates "Title, Description, and Topic" (Fold (to (textUpdate "Title" "PerfTest2"))
                    <|> Fold (to (paragraphUpdate "Description" "A description goes in here."))
                    <|> Fold (to (dropdownUpdate "Topic" 11))
                    )
    >>= sendUpdates "Rest" (Fold (to (dropdownUpdate "Subtopic" 2))
                    <|> Fold (to (dropdownUpdate "Priority" 3))
                    <|> Fold (to (dropdownUpdate "Inquiry Type" 2))
                    <|> Fold (to (textUpdate "First Name" "This is my first name!"))
                    <|> Fold (to (textUpdate "Last Name" "This is my last name!"))
                    <|> Fold (to (textUpdate "Email" "somename@someplace.com"))
                    <|> Fold (to (textUpdate "Phone" "0123456789"))
                    <|> Fold (to (buttonUpdate "Submit"))
                    )
  case v ^? deep (filtered $ has $ key "#v" . _String . suffixed "has been created") . key "#v" . _String of
    Nothing -> fail "The case was not created!"
    Just txt -> return txt

getTaskProcId :: (Contravariant f, Applicative f, Plated s, AsValue s) => Text -> (Result ProcessModelId -> f (Result ProcessModelId)) -> s -> f s
getTaskProcId label = hasKeyValue "displayLabel" label . key "processModelId" . to fromJSON . to (fmap ProcessModelId)

handleResult :: Result a -> Appian a
handleResult (Error err) = fail err
handleResult (Success a) = pure a

dropdownUpdateWith :: (ToUpdate a, Contravariant f, Applicative f, Plated s, AsValue s, AsJSON s) => (DropdownField -> a) -> Text -> (Update -> f Update) -> s -> f s
dropdownUpdateWith f label = getDropdown label . to f . to toUpdate

dropdownRandom :: MonadRandom m => DropdownField -> m DropdownField
dropdownRandom df = do
  let n = df ^. dfChoices . to length
  idx <- getRandomR (1, n+1)
  return $ dfValue .~ idx $ df
