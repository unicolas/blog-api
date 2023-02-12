{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.PostStore (PostStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.UUID.V4 (nextRandom)
import Mocks.StorageMock (StorageMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Post (Post(..))
import Models.Types.Cursor (Cursor(..))
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Sorting)
import Models.User (User)
import Stores.PostStore (PostStore(..))
import Utils (dropEntitiesBefore, sortEntitiesBy)

instance PostStore StorageMock where
  find :: Id Post -> StorageMock (Maybe (Entity Post))
  find postId = gets (Map.lookup postId . StorageMock.posts)

  findAll :: Sorting -> Maybe Cursor -> Int -> StorageMock [Entity Post]
  findAll sorting maybeCursor n = gets
    $ take n
    . dropEntitiesBefore maybeCursor
    . sortEntitiesBy sorting
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

  findByAuthor :: Id User -> Sorting -> Maybe Cursor -> Int -> StorageMock [Entity Post]
  findByAuthor author sorting maybeCursor n = gets
    $ take n
    . dropEntitiesBefore maybeCursor
    . sortEntitiesBy sorting
    . filter authored
    . Map.elems
    . StorageMock.posts
    where
      authored (Entity _ post) = author == userId post
