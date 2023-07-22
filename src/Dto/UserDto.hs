{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.UserDto (UserDto(..), NewUserDto(..), fromEntity, UserIdDto(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Types.Entity (Entity(..))
import qualified Models.Types.Id as Id
import Models.User (User(..))

data UserDto = UserDto
  { userId :: !UUID
  , username :: !Text
  , email :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON)

fromEntity :: Entity User -> UserDto
fromEntity (Entity userId User {..}) = UserDto {userId = Id.unwrap userId, ..}

data NewUserDto = NewUserDto
  { username :: !Text
  , email :: !Text
  , password :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

newtype UserIdDto = UserIdDto {userId :: UUID}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON
