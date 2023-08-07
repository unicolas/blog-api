{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.UsernameSpec (spec) where

import Data.Either (isLeft)
import Data.Text (pack)
import Models.Username (Username(Username), makeUsername)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Gen, arbitrary, chooseInt, forAll, shuffle, sized, suchThat, vectorOf)
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = do
  describe "makeUsername" $ do
    prop "Valid lenght and content produces a username" $
      forAll username $ \x -> makeUsername x `shouldSatisfy` wraps x

    prop "Fails with an invalid char" $
      forAll invalidUsername $ \x -> makeUsername x `shouldSatisfy` isLeft

    it "Fails when empty" $
      makeUsername "" `shouldSatisfy` isLeft

  where
    wraps x = \case
      Right (Username u) -> u == x
      Left _ -> False
    isValid = (`elem` ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['-','_'])
    username = pack <$> listN 4 (arbitrary `suchThat` isValid)
    invalidUsername = pack <$> do
      invalid <- arbitrary `suchThat` (not . isValid)
      validList <- listN 0 (arbitrary `suchThat` isValid)
      shuffle (invalid : validList)

listN :: Int -> Gen Char -> Gen [Char]
listN n gen = sized $ \m -> do
  k <- chooseInt (n, max n m)
  vectorOf k gen
