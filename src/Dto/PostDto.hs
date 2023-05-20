{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.PostDto
  ( PostDto(..)
  , PostIdDto(..)
  , NewPostDto(..)
  , toPost
  , toPostId
  , fromAggregate
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Post (Post(..))
import Models.Tag (Tag(..))
import Models.Types.Aggregate (Aggregate(..))
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
  , tags :: ![Text]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

fromAggregate :: Aggregate Post [Tag] -> PostDto
fromAggregate (Aggregate (Entity (Id pid) post) tags) = fromPost post
  where
    fromPost
      Post
        { userId = Id aid
        , ..
        }
      = PostDto
        { postId = pid
        , authorId = aid
        , tags = term <$> tags
        , ..
        }

newtype PostIdDto = PostIdDto {postId :: UUID}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

toPostId :: PostIdDto -> Id Post
toPostId PostIdDto{..} = Id postId

data NewPostDto = NewPostDto
  { title :: !Text
  , content :: !Text
  , tags :: !(Maybe [Text])
  }
  deriving (Show, Eq, Generic, FromJSON)

toPost :: NewPostDto -> Id User -> UTCTime -> UTCTime -> Post
toPost NewPostDto {..} userId createdAt updatedAt = Post {..}
