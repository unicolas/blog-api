{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.UserDto (UserDto(..), fromEntity) where

import Data.Aeson (ToJSON)
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
