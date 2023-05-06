{-# LANGUAGE DeriveAnyClass #-}

module Models.User (User(..)) where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)

data User = User
  { username :: !Text
  , email:: !Text
  }
  deriving
    ( Show
    , Eq
    , Generic
    , FromRow
    , ToRow
    , FromJSON
    , ToJSON
    , FromJWT
    , ToJWT
    )
