{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User)

data CommentDto = CommentDto
  { commentId :: !UUID
  , title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , postId :: !UUID
  , authorId :: !UUID
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

fromEntity :: Entity Comment -> CommentDto
fromEntity (Entity (Id cid) comment) = fromComment comment
  where
    fromComment
      Comment
        { postId = (Id pid)
        , userId = (Id aid)
        , ..
        }
      = CommentDto
        { commentId = cid
        , postId = pid
        , authorId = aid
        , ..
        }

newtype CommentIdDto = CommentIdDto {commentId :: UUID}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toCommentId :: CommentIdDto -> Id Comment
toCommentId CommentIdDto{..} = Id commentId

data NewCommentDto = NewCommentDto
  { title :: !Text
  , content :: !Text
  , postId :: !UUID
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toComment :: NewCommentDto  -> Id User ->UTCTime -> UTCTime ->  Comment
toComment NewCommentDto {postId = pid , ..} userId createdAt updatedAt
  = Comment {postId = Id pid, ..}
