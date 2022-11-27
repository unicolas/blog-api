{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.CommentDto (CommentDto(..), fromEntity) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Comment (Comment(..))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))

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
