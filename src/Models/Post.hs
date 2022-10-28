{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Post (Post(..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Post = Post
  { title :: !Text
  , content :: !Text
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  }
  deriving (Show, Generic, FromRow, ToRow)
