{-# LANGUAGE DeriveAnyClass #-}

module Models.Comment (Comment(..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.Post (Post)
import Models.Types.Id (Id)
import Models.User (User)

data Comment = Comment
  { title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , postId :: !(Id Post)
  , userId :: !(Id User)
  , parentId :: !(Maybe (Id Comment))
  }
  deriving (Show, Eq, Generic, FromRow, ToRow)
