{-# LANGUAGE OverloadedStrings #-}

module Models.EmailSpec (spec) where

import Data.Either (isLeft)
import Data.Text (pack)
import Generators (listOfN)
import Models.Email (Email(Email), makeEmail)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (arbitrary, chooseEnum, forAll, frequency, listOf, suchThat)

spec :: Spec
spec = do
  describe "makeEmail" $ do
    prop "Valid content produces an Email" $
      forAll email $ \x -> makeEmail x `shouldSatisfy` wraps x

    prop "Fails with invalid content" $
      forAll invalidEmail $ \x -> makeEmail x `shouldSatisfy` isLeft

    it "Fails when empty" $
      makeEmail "" `shouldSatisfy` isLeft

    where
      wraps x = either (const False) (\(Email e) -> e == x)
      email = pack <$> do
        a <- listOfN 2 (arbitrary `suchThat` (/= '@'))
        b <- listOfN 2 (arbitrary `suchThat` (/= '.'))
        c <- listOfN 2 arbitrary
        pure (mconcat [a, ['@'], b, ['.'], c])
      invalidEmail = pack <$> do
        selectAt <- chooseEnum (True, False)
        let (sel, excl) = if selectAt then ('@', '.') else ('.', '@')
        listOf $ frequency
          [ (1, arbitrary `suchThat` (== sel))
          , (8, arbitrary `suchThat` (/= excl))
          ]
