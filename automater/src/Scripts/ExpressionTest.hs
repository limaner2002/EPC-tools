{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ClassyPrelude hiding (assert)
import Appian
import Appian.Client
import Appian.Lens
import Appian.Types
import Appian.Instances
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Scripts.Test
import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Reified
import Data.Aeson
import Servant.Client (ClientEnv)
import Formatting

newtype ExpressionList = ExpressionList [Integer]
  deriving (Show, Monoid)

newtype Expression = Expression Text

instance Arbitrary ExpressionList where
  arbitrary = ExpressionList <$> arbitrary

dispList :: ExpressionList -> Text
dispList (ExpressionList l) = dispList_ l (length l)

dispList_ :: [Integer] -> Int -> Text
dispList_ _ 0 = "List of Variant: " <> "0 items"
dispList_ l 1 = "List of Number (Integer): " <> "1 item\n    " <> intercalate "\n    " (fmap tshow l)
dispList_ l n = "List of Number (Integer): " <> (toStrict $ format commas n) <> " items\n    " <>  intercalate "\n    " (fmap tshow l)

toAppianList :: ExpressionList -> Text
toAppianList (ExpressionList l) = "{" <> intercalate "," (fmap tshow l) <> "}"

arbitraryExpressionList :: MonadGen m => ExpressionEditorWidget -> m ExpressionEditorWidget
arbitraryExpressionList widget = do
  l <- genArbitrary arbitrary
  return $ expwValue .~ toAppianList l $ widget

expressionListArbitrary :: (Applicative f, Effective m r f, MonadGen m) => (Either Text Update -> f (Either Text Update)) -> Value -> f Value
expressionListArbitrary = failing (getExpressionInfoPanel . traverse . editor . to Right) (to $ const $ Left $ "Could not find Expression Editor Widget ") . act (mapM $ arbitraryExpressionList) . to (fmap toUpdate)

expressionListArbitraryF :: MonadGen m => ReifiedMonadicFold m Value (Either Text Update)
expressionListArbitraryF = MonadicFold expressionListArbitrary

testRule :: RapidFire m => Expression -> AppianT m Value
testRule (Expression expr) = do
  v <- newRule
  assign appianValue v
  sendRuleUpdates "Sending blank expression" (MonadicFold $ getExpressionInfoPanel . traverse . editor . to (expwValue .~ "") . to toUpdate . to Right)
  sendRuleUpdates "Sending arbitrary list" (MonadicFold $ getExpressionInfoPanel . traverse . editor . to (expwValue .~ expr) . to toUpdate . to Right)
  sendRuleUpdates "Click 'Test Rule'" (MonadicFold $ to $ buttonUpdate "Test Rule")
  use appianValue  

prop_ruleTest :: Expression -> Text -> LogMode -> AppianState -> ClientEnv -> Login -> Property
prop_ruleTest expr txtGold logMode state env login = monadicIO $ do
  v <- run $ runAppianT logMode (testRule expr) state env login
  txtList <- case v ^? _Right . _Right . getParagraphField "Value" . pgfValue of
    Nothing -> fail "Could not get the expression from the response!"
    Just txt -> return txt

  assert (txtList == txtGold)

testReverseList :: RapidFire m => ExpressionList -> AppianT m Value
testReverseList l = testRule $ Expression $ "fn!reverse(fn!reverse(" <> toAppianList l <> "))"

prop_reverseList' :: LogMode -> AppianState -> ClientEnv -> Login -> ExpressionList -> Property
prop_reverseList' logMode state env login l = prop_ruleTest (Expression $ "fn!reverse(fn!reverse(" <> toAppianList l <> "))") (dispList l) logMode state env login

prop_reverseList :: LogMode -> AppianState -> ClientEnv -> Login -> ExpressionList -> Property
prop_reverseList logMode state env login l = monadicIO $ do
  v <- run $ runAppianT logMode (testReverseList l) state env login
  txtList <- case v ^? _Right . _Right . getParagraphField "Value" . pgfValue of
    Nothing -> fail "Could not get the expression from the response!"
    Just txt -> return txt

  assert (txtList == dispList l)

testSortList :: RapidFire m => ExpressionList -> AppianT m Value
testSortList l = testRule $ Expression $ "fn!sort(" <> toAppianList l <> ")"

sortExpressionList :: ExpressionList -> ExpressionList
sortExpressionList (ExpressionList l) = ExpressionList (sort l)

prop_sortList :: LogMode -> AppianState -> ClientEnv -> Login -> ExpressionList -> Property
prop_sortList logMode state env login l = monadicIO $ do
  v <- run $ runAppianT logMode (testSortList l) state env login
  txtList <- case v ^? _Right . _Right . getParagraphField "Value" . pgfValue of
    Nothing -> fail "Could not get the expression from the response!"
    Just txt -> return txt

  assert (txtList == (dispList $ sortExpressionList l))
