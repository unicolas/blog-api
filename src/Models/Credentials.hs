{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Models.Credentials (Credentials(..)) where

import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.HashedPassword (HashedPassword)
import Models.Types.Id (Id)
import Models.User (User)

data Credentials = Credentials
  { userId :: !(Id User)
  , password :: !HashedPassword
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)
