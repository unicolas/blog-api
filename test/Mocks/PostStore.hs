{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Mocks.PostStore (PostStore(..), Posts(..), StorageMock, runMock) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(..), get, gets, put)
import Data.UUID.V4 (nextRandom)
import Models.Post (Post(..))
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.User (User)
import Stores.PostStore (PostStore(..))

newtype Posts = Posts { unwrap :: [(Id Post, Entity Post)] }

type StorageMock = StateT Posts IO

runMock :: StorageMock a -> Posts -> IO a
runMock st posts = fst <$> runStateT st posts

instance PostStore StorageMock where
  find :: Id Post -> StorageMock (Maybe (Entity Post))
  find postId = gets (lookup postId . unwrap)

  findAll :: StorageMock [Entity Post]
  findAll = gets (fmap snd . unwrap)

  save :: Post -> StorageMock (Maybe (Id Post))
  save post = do
    posts <- get
    postId <- liftIO (Id <$> nextRandom)
    put $ Posts $ (postId, Entity postId post) : unwrap posts
    pure $ Just postId

  delete :: Id Post -> StorageMock ()
  delete _ = undefined

  findByAuthor :: Id User -> StorageMock [Entity Post]
  findByAuthor author =
    gets (\posts -> [ post | post <- snd <$> unwrap posts, authored post ])
    where
      authored (Entity _ post) = author == userId post
