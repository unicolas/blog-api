{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.UserDto (UserDto(..), NewUserDto(..), fromEntity, UserIdDto(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Models.Email (Email)
import Models.Password (Password)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import Models.User (User(..))
import Models.Username (Username)

data UserDto = UserDto
  { userId :: !(Id User)
  , username :: !Username
  , email :: !Email
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

fromEntity :: Entity User -> UserDto
fromEntity (Entity userId User {..}) = UserDto {..}

data NewUserDto = NewUserDto
  { username :: !Username
  , email :: !Email
  , password :: !Password
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

newtype UserIdDto = UserIdDto {userId :: Id User}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON
