{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.PostStore (PostStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.UUID.V4 (nextRandom)
import Mocks.StorageMock (StorageMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Post (Post(..))
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Order(..), Sort(..), Sorting)
import Models.User (User)
import Stores.PostStore (PostStore(..))

instance PostStore StorageMock where
  find :: Id Post -> StorageMock (Maybe (Entity Post))
  find postId = gets (Map.lookup postId . StorageMock.posts)

  findAll :: Sorting -> StorageMock [Entity Post]
  findAll sorting = gets
    $ applySorting sorting
    . Map.elems
    . StorageMock.posts

  save :: Post -> StorageMock (Maybe (Id Post))
  save post = do
    postId <- liftIO (Id <$> nextRandom)
    posts <- gets (Map.insert postId (Entity postId post) . StorageMock.posts)
    modify (\s -> s {StorageMock.posts = posts})
    pure $ Just postId

  delete :: Id Post -> StorageMock ()
  delete postId = do
    posts <- gets (Map.delete postId . StorageMock.posts)
    modify (\s -> s {StorageMock.posts = posts})

  findByAuthor :: Id User -> Sorting -> StorageMock [Entity Post]
  findByAuthor author sorting = gets
    $ applySorting sorting
    . filter authored
    . Map.elems
    . StorageMock.posts
    where
      authored (Entity _ post) = author == userId post

applySorting :: Sorting -> [Entity Post] -> [Entity Post]
applySorting (sort, order) = applySort sort . applyOrder order

applySort :: Sort -> [Entity Post] -> [Entity Post]
applySort = sortBy . \case
  CreatedAt -> comparing (createdAt . post)
  UpdatedAt -> comparing (updatedAt . post)
  Title -> comparing (title . post)
  where post (Entity _ p) = p

applyOrder :: Order -> [Entity Post] -> [Entity Post]
applyOrder = \case
  Asc -> id
  Desc -> reverse
