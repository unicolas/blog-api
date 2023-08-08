module Generators (listOfN) where

import Test.QuickCheck (Gen, chooseInt, sized, vectorOf)

-- | Generates a minimum-sized list of random length. The maximum length depends
-- on the size parameter.
listOfN :: Int -> Gen a -> Gen [a]
listOfN n gen = sized $ \m -> do
  k <- chooseInt (n, max n m)
  vectorOf k gen
