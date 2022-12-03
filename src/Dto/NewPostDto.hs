{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.NewPostDto (NewPostDto(..), toPost) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Models.Post (Post(..))
import Models.Types.Id (Id(..))
import Models.User (User)

data NewPostDto = NewPostDto
  { title :: !Text
  , content :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toPost :: NewPostDto -> Id User -> UTCTime -> UTCTime -> Post
toPost NewPostDto {..} userId createdAt updatedAt = Post {..}
