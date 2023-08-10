{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.UserDto (UserDto(..), NewUserDto(..), fromEntity, UserIdDto(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Email (Email)
import Models.Password (Password)
import Models.Types.Entity (Entity(..))
import qualified Models.Types.Id as Id
import Models.User (User(..))
import Models.Username (Username)

data UserDto = UserDto
  { userId :: !UUID
  , username :: !Username
  , email :: !Email
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

fromEntity :: Entity User -> UserDto
fromEntity (Entity userId User {..}) = UserDto {userId = Id.unwrap userId, ..}

data NewUserDto = NewUserDto
  { username :: !Username
  , email :: !Email
  , password :: !Password
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

newtype UserIdDto = UserIdDto {userId :: UUID}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON
