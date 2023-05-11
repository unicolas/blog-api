module Models.Types.Pagination
  ( Pagination(..)
  , defaultPagination
  , withSort
  , withOrder
  , withCount
  , withCursor
  ) where

import Models.Types.Cursor (Cursor)
import Models.Types.Sorting (Order(..), Sort(..))

data Pagination = Pagination
  { sort :: !Sort
  , order :: !Order
  , count :: !Int
  , cursor :: !(Maybe Cursor)
  }

defaultPagination :: Pagination
defaultPagination = Pagination
  { sort = CreatedAt
  , order = Desc
  , count = 11
  , cursor = Nothing
  }

withSort :: Maybe Sort -> Pagination -> Pagination
withSort s p = maybe p (\sort -> p { sort = sort }) s

withOrder :: Maybe Order -> Pagination -> Pagination
withOrder o p = maybe p (\order -> p { order = order }) o

withCount :: Maybe Int -> Pagination -> Pagination
withCount c p = maybe p (\count -> p { count = count }) c

withCursor :: Maybe Cursor -> Pagination -> Pagination
withCursor c p = p { cursor = c }
