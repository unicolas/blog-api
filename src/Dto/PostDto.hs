{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Dto.PostDto
  ( PostDto(..)
  , PostIdDto(..)
  , NewPostDto(..)
  , toPost
  , fromAggregate
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Models.Post (Post(..))
import Models.Tag (Tag(..))
import Models.Types.Aggregate (Aggregate(..))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User)

data PostDto = PostDto
  { postId :: !(Id Post)
  , title :: !Text
  , content :: !Text
  , authorId :: !(Id User)
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , tags :: ![Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

fromAggregate :: Aggregate Post [Tag] -> PostDto
fromAggregate
  (Aggregate
    (Entity postId Post {userId = authorId, ..})
    (fmap term -> tags)
  ) = PostDto {..}

newtype PostIdDto = PostIdDto {postId :: Id Post}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data NewPostDto = NewPostDto
  { title :: !Text
  , content :: !Text
  , tags :: !(Maybe [Text])
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

toPost :: NewPostDto -> Id User -> UTCTime -> UTCTime -> Post
toPost NewPostDto {..} userId createdAt updatedAt = Post {..}
