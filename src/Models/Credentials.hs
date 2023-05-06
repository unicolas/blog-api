{-# LANGUAGE DeriveAnyClass #-}

module Models.Credentials (Credentials(..)) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.Types.Id (Id)
import Models.User (User)

data Credentials = Credentials
  { userId :: !(Id User)
  , password :: !Text
  }
  deriving (Show, Eq, Generic, FromRow, ToRow)
