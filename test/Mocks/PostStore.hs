{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.PostStore (PostStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.UUID.V4 (nextRandom)
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock
import Models.Post (Post(..))
import Models.Types.Cursor (Cursor(..))
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Sorting)
import Models.User (User)
import Stores.PostStore (PostStore(..))
import Utils (dropEntitiesBefore, sortEntitiesBy)

instance PostStore AppMock where
  find :: Id Post -> AppMock (Maybe (Entity Post))
  find postId = gets (Map.lookup postId . AppMock.posts)

  findAll :: Sorting -> Maybe Cursor -> Int -> AppMock [Entity Post]
  findAll sorting maybeCursor n = gets
    $ take n
    . dropEntitiesBefore maybeCursor
    . sortEntitiesBy sorting
    . Map.elems
    . AppMock.posts

  save :: Post -> AppMock (Maybe (Id Post))
  save post = do
    postId <- liftIO (Id <$> nextRandom)
    posts <- gets (Map.insert postId (Entity postId post) . AppMock.posts)
    modify (\s -> s {AppMock.posts = posts})
    pure $ Just postId

  delete :: Id Post -> AppMock ()
  delete postId = do
    posts <- gets (Map.delete postId . AppMock.posts)
    modify (\s -> s {AppMock.posts = posts})

  findByAuthor :: Id User -> Sorting -> Maybe Cursor -> Int -> AppMock [Entity Post]
  findByAuthor author sorting maybeCursor n = gets
    $ take n
    . dropEntitiesBefore maybeCursor
    . sortEntitiesBy sorting
    . filter authored
    . Map.elems
    . AppMock.posts
    where
      authored (Entity _ post) = author == userId post
