{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.Page (Page(..), make, defaultPageSize) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Models.Types.Cursor (Cursor)
import qualified Models.Types.Cursor as Cursor

data Page dto = Page
  { content :: ![dto]
  , nextCursor :: !(Maybe Text)
  , hasNextPage :: !Bool
  , pageSize :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

make :: [dto] -> Maybe Cursor -> Int -> Page dto
make dtos cursor pageSize = Page {..}
  where
    content = take pageSize dtos
    hasNextPage = length dtos > pageSize
    nextCursor = if hasNextPage then Cursor.encode <$> cursor else Nothing

defaultPageSize :: Int
defaultPageSize = 10
