{-# LANGUAGE PatternSynonyms #-}

module Models.HashedPassword
  ( HashedPassword(HashedPassword)
  , hashPassword
  , unsafeHashedPassword
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Password.Bcrypt as Bcrypt
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField, fromField)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Models.Password (Password(Password))

newtype HashedPassword = MakeHashedPassword Text
  deriving (Eq, Show)

pattern HashedPassword :: Text -> HashedPassword
pattern HashedPassword e <- MakeHashedPassword e
{-# COMPLETE HashedPassword #-}

-- | Hashes a Password using bcrypt
hashPassword :: MonadIO m => Password -> m HashedPassword
hashPassword (Password pswd) = do
  (Bcrypt.PasswordHash hash) <- Bcrypt.hashPassword (Bcrypt.mkPassword pswd)
  pure (MakeHashedPassword hash)

instance ToField HashedPassword where
  toField :: HashedPassword -> Action
  toField (HashedPassword value) = toField value

instance FromField HashedPassword where
  fromField :: FieldParser HashedPassword
  fromField f s = MakeHashedPassword <$> fromField f s

unsafeHashedPassword :: Text -> HashedPassword
unsafeHashedPassword = MakeHashedPassword
