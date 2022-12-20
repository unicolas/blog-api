{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.CommentStore (CommentStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.UUID.V4 (nextRandom)
import Mocks.StorageMock (StorageMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Order(..), Sort(..), Sorting)
import Stores.CommentStore (CommentStore(..))

instance CommentStore StorageMock where
  find :: Id Comment -> StorageMock (Maybe (Entity Comment))
  find commentId = gets (Map.lookup commentId . StorageMock.comments)

  findAll :: Sorting -> StorageMock [Entity Comment]
  findAll sorting = gets
    $ applySorting sorting
    . Map.elems
    . StorageMock.comments

  save :: Comment -> StorageMock (Maybe (Id Comment))
  save comment = do
    commentId <- liftIO (Id <$> nextRandom)
    comments <- gets
      $ Map.insert commentId (Entity commentId comment) . StorageMock.comments
    modify (\s -> s {StorageMock.comments = comments})
    pure $ Just commentId

  delete :: Id Comment -> StorageMock ()
  delete commentId = do
    comments <- gets (Map.delete commentId . StorageMock.comments)
    modify (\s -> s {StorageMock.comments = comments})

  findByPost :: Id Post -> Sorting -> StorageMock [Entity Comment]
  findByPost idPost sorting = gets
    $ applySorting sorting
    . filter forPost
    . Map.elems
    . StorageMock.comments
    where
      forPost (Entity _ comment) = idPost == postId comment

applySorting :: Sorting -> [Entity Comment] -> [Entity Comment]
applySorting (sort, order) = applyOrder order . applySort sort

applySort :: Sort -> [Entity Comment] -> [Entity Comment]
applySort = sortBy . \case
  CreatedAt -> comparing (createdAt . comment)
  UpdatedAt -> comparing (updatedAt . comment)
  Title -> comparing (title . comment)
  where comment (Entity _ c) = c

applyOrder :: Order -> [Entity Comment] -> [Entity Comment]
applyOrder = \case
  Asc -> id
  Desc -> reverse
