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
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.User (User)
import Stores.PostStore (PostStore(..))

instance PostStore StorageMock where
  find :: Id Post -> StorageMock (Maybe (Entity Post))
  find postId = gets (Map.lookup postId . StorageMock.posts)

  findAll :: StorageMock [Entity Post]
  findAll = gets (Map.elems . StorageMock.posts)

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

  findByAuthor :: Id User -> StorageMock [Entity Post]
  findByAuthor author = gets (filter authored . Map.elems . StorageMock.posts)
    where
      authored (Entity _ post) = author == userId post
