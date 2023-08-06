{-# LANGUAGE PatternSynonyms #-}

module Models.Email (Email(Email), makeEmail) where

import Control.Arrow (second, (>>>))
import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Aeson.Types (Parser, Value)
import Data.Text (Text)
import qualified Data.Text as Text

newtype Email = MakeEmail Text
  deriving (Eq, Show)

pattern Email :: Text -> Email
pattern Email e <- MakeEmail e
{-# COMPLETE Email #-}

instance FromJSON Email where
  parseJSON :: Value -> Parser Email
  parseJSON = withText "Email" $ \text -> either fail pure (makeEmail text)

-- | Parses an email of the form x@y.z where all three x, y and z are at least 2
-- characters long.
makeEmail :: Text -> Either String Email
makeEmail text = if valid text then Right (MakeEmail text) else Left "invalid email"
  where
    valid = Text.break (== '@')
      >>> second (Text.break (== '.'))
      >>> \(a, (b, c)) -> (Text.length a > 1) && all (> 2) (Text.length <$> [b, c])

