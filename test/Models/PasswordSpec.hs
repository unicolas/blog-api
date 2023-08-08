{-# LANGUAGE OverloadedStrings #-}

module Models.PasswordSpec (spec) where

import Data.Char (isAlpha, isDigit, isSymbol)
import Data.Either (isLeft)
import Data.Text (pack)
import Generators (listOfN)
import Models.Password (Password(Password), makePassword)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (arbitrary, chooseInt, forAll, listOf, oneof, shuffle, suchThat)

spec :: Spec
spec = do
  describe "makePassword" $ do
    prop "Valid lenght and content produces a password" $
      forAll password $ \x -> makePassword x `shouldSatisfy` wraps x

    prop "Fails with an invalid char" $
      forAll invalidPassword $ \x -> makePassword x `shouldSatisfy` isLeft

    prop "Fails without all three char types" $
      forAll invalidPassword' $ \x -> makePassword x `shouldSatisfy` isLeft

    it "Fails when empty" $
      makePassword "" `shouldSatisfy` isLeft

  where
    wraps x = either (const False) (\(Password p) -> p == x)
    isValid c = c >= '!' && c <= '~'
    alphaGen = arbitrary `suchThat` (\c -> isValid c && isAlpha c)
    digitGen = arbitrary `suchThat` (\c -> isValid c && isDigit c)
    symbolGen = arbitrary `suchThat` (\c -> isValid c && isSymbol c)
    validGen = arbitrary `suchThat` isValid
    invalidGen = arbitrary `suchThat` (not . isValid)
    password = pack <$> do
      alpha <- alphaGen
      digit <- digitGen
      symbol <- symbolGen
      rest <- listOfN 3 validGen
      shuffle (alpha : digit : symbol : rest)
    invalidPassword = pack <$> do
      invalid <- invalidGen
      rest <- listOf $ oneof [alphaGen, digitGen, symbolGen, invalidGen]
      shuffle (invalid : rest)
    invalidPassword' = pack <$> do
      k <- chooseInt (0, 2)
      listOf $ (oneof . take 2 . drop k) [alphaGen, digitGen, symbolGen, alphaGen]
