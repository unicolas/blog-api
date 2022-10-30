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
  , authorId :: !UUID
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

fromEntity :: Entity Post -> PostDto
fromEntity (Entity (Id pid) post) = fromPost post
  where
    fromPost
      Post
        { userId = Id aid
        , ..
        }
      = PostDto
        { postId = pid
        , authorId = aid
        , ..
        }

toPost :: PostDto -> Post
toPost PostDto {authorId = uuid, ..} = Post {userId = Id uuid, ..}
