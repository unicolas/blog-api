{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
  ( makeId
  , makeUtc
  , makeUuid
  , serverError
  , sortEntitiesBy
  , dropEntitiesBefore
  , emptyPage
  , getUuid
  ) where

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.UUID (UUID, fromString)
import Dto.Page (Page(..), defaultPageSize)
import GHC.Records (HasField, getField)
import Models.Types.Cursor (Cursor(..))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Order(..), Sort(..), Sorting)
import Servant (ServerError(..))
import Test.Hspec (Selector)

makeUtc :: String -> UTCTime
makeUtc = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"

makeUuid :: String -> UUID
makeUuid = fromJust . fromString

makeId :: String -> Id phantom
makeId = Id . makeUuid

-- | Makes a `Selector` for `ServerError` exceptions by HTTP error code
serverError :: Int -> Selector ServerError
serverError code ServerError{errHTTPCode} = code == errHTTPCode

-- | Sorts a list of entities by `Sorting`.
sortEntitiesBy ::
  ( HasField "createdAt" model UTCTime
  , HasField "updatedAt" model UTCTime
  , HasField "title" model Text
  ) => Sorting -> [Entity model] -> [Entity model]
sortEntitiesBy (sort, order) = applyOrder order . applySort sort
  where
    model (Entity _ m) = m
    applySort = sortBy . \case
      CreatedAt -> comparing (getField @"createdAt" . model)
      UpdatedAt -> comparing (getField @"updatedAt" . model)
      Title -> comparing (getField @"title" . model)
    applyOrder = \case
      Asc -> id
      Desc -> reverse

-- | Discards entities preceding `Cursor`.
dropEntitiesBefore ::
  ( HasField "createdAt" model UTCTime
  , HasField "updatedAt" model UTCTime
  , HasField "title" model Text
  ) => Maybe Cursor -> [Entity model] -> [Entity model]
dropEntitiesBefore maybeCursor = dropWhile $ maybe (const False) precedes maybeCursor
  where
    precedes cursor (Entity (Id i) model) = case cursor of
      CreatedAtCursor v o -> cmp v (getField @"createdAt" model, Id i) o
      UpdatedAtCursor v o -> cmp v (getField @"updatedAt" model, Id i) o
      TitleCursor v o -> cmp v (getField @"title" model, Id i) o
    cmp a b = \case
        Asc -> a > b
        Desc -> a < b

emptyPage :: Page dto
emptyPage = Page [] Nothing False defaultPageSize

getUuid :: Id phantom -> UUID
getUuid (Id uuid) = uuid
