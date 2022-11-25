{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.NewPostDto (NewPostDto(..), toPost) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Post (Post(..))
import Models.Types.Id (Id(..))

data NewPostDto = NewPostDto
  { title :: !Text
  , content :: !Text
  , authorId :: !UUID
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toPost :: NewPostDto -> Post
toPost NewPostDto {authorId = uuid, ..} = Post {userId = Id uuid, ..}
