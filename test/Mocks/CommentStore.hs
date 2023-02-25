{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.CommentStore (CommentStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.UUID.V4 (nextRandom)
import Mocks.StorageMock (StorageMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Cursor (Cursor)
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Sorting)
import Stores.CommentStore (CommentStore(..))
import Utils (dropEntitiesBefore, sortEntitiesBy)

instance CommentStore StorageMock where
  find :: Id Comment -> StorageMock (Maybe (Entity Comment))
  find commentId = gets (Map.lookup commentId . StorageMock.comments)

  findAll :: Sorting -> Maybe Cursor -> Int -> StorageMock [Entity Comment]
  findAll sorting maybeCursor n = gets
    $ take n
    . dropEntitiesBefore maybeCursor
    . sortEntitiesBy sorting
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

  findByPost :: Id Post -> Sorting -> Maybe Cursor -> Int -> StorageMock [Entity Comment]
  findByPost = findBy
    $ \idPost (Entity _ comment)
      -> idPost == postId comment && isNothing (parentId comment)

  findByComment :: Id Comment -> Sorting -> Maybe Cursor -> Int -> StorageMock [Entity Comment]
  findByComment = findBy
    $ \idComment (Entity _ comment) -> Just idComment == parentId comment

findBy :: (Id model -> Entity Comment -> Bool)
  -> Id model
  -> Sorting
  -> Maybe Cursor
  -> Int
  -> StorageMock [Entity Comment]
findBy p modelId sorting maybeCursor n = gets
  $ take n
  . dropEntitiesBefore maybeCursor
  . sortEntitiesBy sorting
  . filter (p modelId)
  . Map.elems
  . StorageMock.comments
