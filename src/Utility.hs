module Utility (maybeRight, (<<*)) where

import Control.Applicative (liftA2)

-- | Right projection of Either in Maybe
maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

-- | Sequence lifted actions and discard the value of the second argument.
(<<*) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 b) -> f1 (f2 a)
(<<*) = liftA2 (<*)
infixl 4 <<*
