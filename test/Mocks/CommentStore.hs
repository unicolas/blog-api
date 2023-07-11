{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Mocks.CommentStore (CommentStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.UUID.V4 (nextRandom)
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.Types.Pagination (Pagination(..))
import Stores.CommentStore (CommentStore(..))
import Utils (dropEntitiesBefore, sortEntitiesBy)

instance CommentStore AppMock where
  find :: Id Comment -> AppMock (Maybe (Entity Comment))
  find commentId = gets (Map.lookup commentId . AppMock.comments)

  save :: Comment -> AppMock (Maybe (Id Comment))
  save comment = do
    commentId <- liftIO (Id <$> nextRandom)
    comments <- gets
      $ Map.insert commentId (Entity commentId comment) . AppMock.comments
    modify (\s -> s {AppMock.comments = comments})
    pure $ Just commentId

  delete :: Id Comment -> AppMock ()
  delete commentId = do
    comments <- gets (Map.delete commentId . AppMock.comments)
    modify (\s -> s {AppMock.comments = comments})

  findByPost :: Id Post -> Pagination -> AppMock [Entity Comment]
  findByPost = findBy
    $ \idPost (Entity _ comment)
      -> idPost == postId comment && isNothing (parentId comment)

  findByComment :: Id Comment -> Pagination -> AppMock [Entity Comment]
  findByComment = findBy
    $ \idComment (Entity _ comment) -> Just idComment == parentId comment

  countByPost :: Id Post -> AppMock Int
  countByPost idPost = gets
    $ length
    . filter topLevelByPost
    . Map.elems
    . AppMock.comments
    where
      topLevelByPost (Entity _ comment)
        = idPost == postId comment && isNothing (parentId comment)

findBy :: (Id model -> Entity Comment -> Bool)
  -> Id model
  -> Pagination
  -> AppMock [Entity Comment]
findBy p modelId Pagination {..} = gets
  $ take count
  . dropEntitiesBefore cursor
  . sortEntitiesBy (sort, order)
  . filter (p modelId)
  . Map.elems
  . AppMock.comments
