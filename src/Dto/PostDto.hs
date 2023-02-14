{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.PostDto
  ( PostDto(..)
  , PostIdDto(..)
  , NewPostDto(..)
  , fromEntity
  , toPost
  , toPostId
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Post (Post(..))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User)

data PostDto = PostDto
  { postId :: !UUID
  , title :: !Text
  , content :: !Text
  , authorId :: !UUID
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

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

newtype PostIdDto = PostIdDto {postId :: UUID}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toPostId :: PostIdDto -> Id Post
toPostId PostIdDto{..} = Id postId

data NewPostDto = NewPostDto
  { title :: !Text
  , content :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toPost :: NewPostDto -> Id User -> UTCTime -> UTCTime -> Post
toPost NewPostDto {..} userId createdAt updatedAt = Post {..}
