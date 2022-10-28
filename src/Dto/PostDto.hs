{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.PostDto (PostDto(..), fromEntity, toPost) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Post (Post(..))
import Stores.Types.Entity (Entity(..))
import Stores.Types.Id (Id(..))

data PostDto = PostDto
  { postId :: !UUID
  , title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

fromEntity :: Entity Post -> PostDto
fromEntity (Entity (Id uuid) Post {..}) = PostDto {postId = uuid, ..}

toPost :: PostDto -> Post
toPost PostDto {..} = Post {..}
