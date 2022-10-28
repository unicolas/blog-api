{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Comment (Comment(..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.Post (Post)
import Stores.Types.Id (Id)

data Comment = Comment
  { title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , postId :: Id Post
  }
  deriving (Show, Generic, FromRow, ToRow)
