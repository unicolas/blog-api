module Utility (maybeRight) where

-- | Right projection of Either in Maybe
maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just
