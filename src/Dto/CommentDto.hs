{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.CommentDto
  ( CommentDto(..)
  , CommentIdDto(..)
  , NewCommentDto(..)
  , fromEntity
  , toComment
  , toCommentId
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import qualified Models.Types.Id as Id
import Models.User (User)

data CommentDto = CommentDto
  { commentId :: !UUID
  , title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , postId :: !UUID
  , authorId :: !UUID
  , parentId :: !(Maybe UUID)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

fromEntity :: Entity Comment -> CommentDto
fromEntity (Entity commentId comment) = fromComment comment
  where
    fromComment
      Comment
        { userId
        , ..
        }
      = CommentDto
        { commentId = Id.unwrap commentId
        , postId = Id.unwrap postId
        , authorId = Id.unwrap userId
        , parentId = Id.unwrap <$> parentId
        , ..
        }

newtype CommentIdDto = CommentIdDto {commentId :: UUID}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toCommentId :: CommentIdDto -> Id Comment
toCommentId CommentIdDto{..} = Id commentId

data NewCommentDto = NewCommentDto
  { title :: !Text
  , content :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toComment :: NewCommentDto
  -> Id User
  -> Id Post
  -> Maybe (Id Comment)
  -> UTCTime
  -> UTCTime
  -> Comment
toComment NewCommentDto {..} userId postId parentId createdAt updatedAt
  = Comment {..}
