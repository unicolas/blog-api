{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.CommentDto (CommentDto(..), fromEntity, toComment) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Comment (Comment(..))
import Stores.Types.Entity (Entity(..))
import Stores.Types.Id (Id(..))

data CommentDto = CommentDto
  { commentId :: !UUID
  , title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , postId :: !UUID
  , authorId :: !UUID
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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

toComment :: CommentDto -> Comment
toComment
  CommentDto
    { postId = pid
    , authorId = aid
    , ..
    }
  = Comment
    { postId = Id pid
    , userId = Id aid
    , ..
    }
