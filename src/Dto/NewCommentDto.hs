{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.NewCommentDto (NewCommentDto(..), toComment) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Models.Comment (Comment(..))
import Models.Types.Id (Id(..))

data NewCommentDto = NewCommentDto
  { title :: !Text
  , content :: !Text
  , postId :: !UUID
  , authorId :: !UUID
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toComment :: NewCommentDto -> Comment
toComment
  NewCommentDto
    { postId = pid
    , authorId = aid
    , ..
    }
  = Comment
    { postId = Id pid
    , userId = Id aid
    , ..
    }
