{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Models.User (User(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.Email (Email)
import Models.Username (Username)
import Servant.Auth.Server (FromJWT, ToJWT)

data User = User
  { username :: !Username
  , email:: !Email
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow, FromJSON, ToJSON, FromJWT, ToJWT)
