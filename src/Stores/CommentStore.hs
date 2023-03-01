{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stores.CommentStore (CommentStore (..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (fromOnly)
import DatabaseContext (DatabaseContext(..))
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Cursor (Cursor, cursorExpression)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.Types.Sorting (Sorting, sortExpression)
import qualified Stores.Query as Query

class Monad m => CommentStore m where
  find :: Id Comment -> m (Maybe (Entity Comment))
  save :: Comment -> m (Maybe (Id Comment))
  delete :: Id Comment -> m ()
  findByPost :: Id Post -> Sorting -> Maybe Cursor -> Int -> m [Entity Comment]
  findByComment :: Id Comment -> Sorting -> Maybe Cursor -> Int -> m [Entity Comment]

instance CommentStore App where
  find :: Id Comment -> App (Maybe (Entity Comment))
  find idComment = do
    pool <- asks (connectionPool . databaseContext)
    comments <- Query.fetch
        [ "SELECT id, title, content, created_at, updated_at, post_id, user_id, parent_comment_id"
        , "FROM comments"
        , "WHERE id = ?"
        ] [idComment]
      & withResource pool
      & liftIO
    pure (listToMaybe comments)

  save :: Comment -> App (Maybe (Id Comment))
  save Comment{..} = do
    pool <- asks (connectionPool . databaseContext)
    ids <- Query.fetch
        [ "INSERT INTO comments"
        , "(id, title, content, created_at, updated_at, post_id, user_id, parent_comment_id)"
        , "VALUES (gen_random_uuid(), ?, ?, ?, ?, ?, ?, ?)"
        , "RETURNING id"
        ] (title, content, createdAt, updatedAt, postId, userId, parentId)
      & withResource pool
      & liftIO
    pure (fromOnly <$> listToMaybe ids)

  delete :: Id Comment -> App ()
  delete commentId = do
    pool <- asks (connectionPool . databaseContext)
    Query.run ["DELETE FROM comments WHERE id = ?"] [commentId]
      & withResource pool
      & liftIO
      & void

  findByPost :: Id Post -> Sorting -> Maybe Cursor -> Int -> App [Entity Comment]
  findByPost idPost sorting maybeCursor count = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        [ "SELECT id, title, content, created_at, updated_at, post_id, user_id, parent_comment_id"
        , "FROM comments"
        , "WHERE post_id = ?"
        , maybe mempty (("AND " <>) . cursorExpression) maybeCursor
        , "AND parent_comment_id IS NULL"
        , "ORDER BY", sortExpression sorting
        , "FETCH FIRST ? ROWS ONLY"
        ] (idPost, count)
      & withResource pool
      & liftIO

  findByComment :: Id Comment -> Sorting -> Maybe Cursor -> Int -> App [Entity Comment]
  findByComment idComment sorting maybeCursor count = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        [ "SELECT id, title, content, created_at, updated_at, post_id, user_id, parent_comment_id"
        , "FROM comments"
        , "WHERE parent_comment_id = ?"
        , maybe mempty (("AND " <>) . cursorExpression) maybeCursor
        , "ORDER BY", sortExpression sorting
        , "FETCH FIRST ? ROWS ONLY"
        ] (idComment, count)
      & withResource pool
      & liftIO
