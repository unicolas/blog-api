{-# LANGUAGE PatternSynonyms #-}

module Models.Password (Password(Password), makePassword) where

import Control.Arrow (second, (>>>))
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Aeson.Types (Parser, Value)
import Data.Char (isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

newtype Password = MakePassword Text
  deriving (Eq, Show)

pattern Password :: Text -> Password
pattern Password e <- MakePassword e
{-# COMPLETE Password #-}

instance FromJSON Password where
  parseJSON :: Value -> Parser Password
  parseJSON = withText "Password" $ \text -> either fail pure (makePassword text)

-- | Parses a password that is 6 or more characters long and contains at least
-- one letter, one digit and one symbol.
makePassword :: Text -> Either String Password
makePassword = fmap MakePassword . (validateLength >=> validateGroups)
  where
    validateLength pswd = if Text.length pswd > 5
      then Right pswd
      else Left "password length is less than 6 chars"
    validateGroups pswd = if not (emptyGroups pswd)
      then Right pswd
      else Left "invalid password"
    emptyGroups = Text.partition isAlpha
      >>> second (Text.partition isDigit)
      >>> \(alpha, (digit, other)) -> any Text.null [alpha, digit, other]
