{-# LANGUAGE PatternSynonyms #-}

module Models.Username (Username(Username), makeUsername, unsafeUsername) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Aeson.Types (Parser, Value)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

newtype Username = MakeUsername Text
  deriving (Eq, Show)

pattern Username :: Text -> Username
pattern Username e <- MakeUsername e
{-# COMPLETE Username #-}

instance FromJSON Username where
  parseJSON :: Value -> Parser Username
  parseJSON = withText "Username" (either fail pure . makeUsername)

-- | Parses a username that is 4 or more characters long and it's made of letters,
-- digits and '_' or '-'.
makeUsername :: Text -> Either String Username
makeUsername text = if valid text
  then Right (MakeUsername text)
  else Left "invalid username"
  where
    validChar c = isAsciiUpper c
     || isAsciiLower c
     || isDigit c
     || c == '_'
     || c == '-'
    valid = Text.partition validChar
      >>> \(accepted, other) -> (Text.length accepted > 3) && Text.null other

unsafeUsername :: Text -> Username
unsafeUsername = MakeUsername
