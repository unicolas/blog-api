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
import Models.User (User)

data NewCommentDto = NewCommentDto
  { title :: !Text
  , content :: !Text
  , postId :: !UUID
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toComment :: NewCommentDto  -> Id User ->UTCTime -> UTCTime ->  Comment
toComment NewCommentDto {postId = pid , ..} userId createdAt updatedAt
  = Comment {postId = Id pid, ..}
