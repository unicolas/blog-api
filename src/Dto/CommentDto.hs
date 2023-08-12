{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.CommentDto
  ( CommentDto(..)
  , CommentIdDto(..)
  , NewCommentDto(..)
  , fromEntity
  , toComment
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User)

data CommentDto = CommentDto
  { commentId :: !(Id Comment)
  , title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , postId :: !(Id Post)
  , authorId :: !(Id User)
  , parentId :: !(Maybe (Id Comment))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

fromEntity :: Entity Comment -> CommentDto
fromEntity (Entity commentId Comment {userId = authorId, ..}) = CommentDto {..}

newtype CommentIdDto = CommentIdDto {commentId :: Id Comment}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data NewCommentDto = NewCommentDto
  { title :: !Text
  , content :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

toComment :: NewCommentDto
  -> Id User
  -> Id Post
  -> Maybe (Id Comment)
  -> UTCTime
  -> UTCTime
  -> Comment
toComment NewCommentDto {..} userId postId parentId createdAt updatedAt
  = Comment {..}
