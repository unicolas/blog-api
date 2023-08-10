{-# LANGUAGE OverloadedStrings #-}

module Models.UsernameSpec (spec) where

import Data.Either (isLeft)
import Data.Text (pack)
import Generators (listOfN)
import Models.Username (Username(Username), makeUsername)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, forAll, listOf, shuffle, suchThat)
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
    wraps x = either (const False) (\(Username u) -> u == x)
    isValid = (`elem` ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['-','_'])
    username = pack <$> listOfN 4 (arbitrary `suchThat` isValid)
    invalidUsername = pack <$> do
      invalid <- arbitrary `suchThat` (not . isValid)
      validList <- listOf (arbitrary `suchThat` isValid)
      shuffle (invalid : validList)
