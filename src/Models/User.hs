{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.User (User(..)) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data User = User
  { username :: !Text
  , email:: !Text
  }
  deriving (Show, Generic, FromRow, ToRow)
