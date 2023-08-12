{-# LANGUAGE PatternSynonyms #-}

module Models.Email (Email(Email), makeEmail, unsafeEmail) where

import Control.Arrow (second, (>>>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON, withText)
import Data.Aeson.Types (Encoding, Parser, Value)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField, fromField)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)

newtype Email = MakeEmail Text
  deriving (Eq, Show)

pattern Email :: Text -> Email
pattern Email e <- MakeEmail e
{-# COMPLETE Email #-}

-- | Parses an email of the form x@y.z where all three x, y and z are at least 2
-- characters long.
makeEmail :: Text -> Either String Email
makeEmail text = if valid text then Right (MakeEmail text) else Left "invalid email"
  where
    valid = Text.break (== '@')
      >>> second (Text.break (== '.'))
      >>> \(a, (b, c)) -> (Text.length a > 1) && all ((> 2) . Text.length) [b, c]

instance FromJSON Email where
  parseJSON :: Value -> Parser Email
  parseJSON = withText "Email" (either fail pure . makeEmail)

instance ToJSON Email where
  toJSON :: Email -> Value
  toJSON (Email e) = toJSON e

  toEncoding :: Email -> Encoding
  toEncoding (Email e) = toEncoding e

instance ToField Email where
  toField :: Email -> Action
  toField (Email value) = toField value

instance FromField Email where
  fromField :: FieldParser Email
  fromField f s = MakeEmail <$> fromField f s

unsafeEmail :: Text -> Email
unsafeEmail = MakeEmail
