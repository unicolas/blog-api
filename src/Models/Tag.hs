{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Models.Tag (Tag(..)) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.Post (Post)
import Models.Types.Id (Id)

data Tag = Tag
  { postId :: !(Id Post)
  , term :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)
