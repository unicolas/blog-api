{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Post (Post(..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.Types.Id (Id)
import Models.User (User)

data Post = Post
  { title :: !Text
  , content :: !Text
  , userId :: !(Id User)
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Eq, Generic, FromRow, ToRow)
