{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Types.Sorting
  (Sort(..), Sorting, Order(..), make, sortExpression) where

import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Query)
import qualified Servant

type Sorting = (Sort, Order)

data Sort = CreatedAt | UpdatedAt | Title
data Order = Asc | Desc
  deriving (Show, Read)

instance Servant.FromHttpApiData Sort where
  parseQueryParam :: Text -> Either Text Sort
  parseQueryParam = \case
    "createdAt" -> Right CreatedAt
    "updatedAt" -> Right UpdatedAt
    "title" -> Right Title
    _ -> Left "Unknown sort option"

instance Servant.FromHttpApiData Order where
  parseQueryParam :: Text -> Either Text Order
  parseQueryParam = \case
    "asc" -> Right Asc
    "desc" -> Right Desc
    _ -> Left "Unknown order option"

make :: Maybe Sort -> Maybe Order -> Sorting
make = curry (fromMaybe CreatedAt *** fromMaybe Desc)

sortExpression :: Sorting -> Query
sortExpression (sort, order) = mconcat
  [ case sort of
      CreatedAt -> "created_at "
      UpdatedAt -> "updated_at "
      Title -> "title "
  , case order of
      Asc -> "asc, id"
      Desc -> "desc, id"
  ]
