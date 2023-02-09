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
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute, fromOnly, query)
import DatabaseContext (DatabaseContext(..))
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Cursor (Cursor, cursorExpression)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.Types.Sorting (Sorting, sortExpression)

class Monad m => CommentStore m where
  find :: Id Comment -> m (Maybe (Entity Comment))
  findAll :: Sorting -> Maybe Cursor -> Int -> m [Entity Comment]
  save :: Comment -> m (Maybe (Id Comment))
  delete :: Id Comment -> m ()
  findByPost :: Id Post -> Sorting -> Maybe Cursor -> Int -> m [Entity Comment]

instance CommentStore App where
  find :: Id Comment -> App (Maybe (Entity Comment))
  find idComment = do
    let sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
            \  FROM comments \
            \  WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    comments <- liftIO $ withResource pool $ \conn -> query conn sql [idComment]
    pure $ listToMaybe comments

  findAll :: Sorting -> Maybe Cursor -> Int -> App [Entity Comment]
  findAll sorting maybeCursor count = do
    let
      sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
          \  FROM comments "
          <> whereCursor maybeCursor <> " \
          \  ORDER BY " <> sortExpression sorting <> " \
          \  FETCH FIRST ? ROWS ONLY"
      whereCursor = maybe mempty (\c -> "WHERE " <> cursorExpression c)
    pool <- asks (connectionPool . databaseContext)
    liftIO $ withResource pool $ \conn -> query conn sql [count]

  save :: Comment -> App (Maybe (Id Comment))
  save Comment{..} = do
    let sql = "INSERT INTO comments \
            \  (id, title, content, created_at, updated_at, post_id, user_id) \
            \  VALUES (gen_random_uuid(), ?, ?, ?, ?, ?, ?) \
            \  RETURNING id"
    pool <- asks $ connectionPool . databaseContext
    ids <- liftIO $ withResource pool
      $ \conn -> query conn sql (title, content, createdAt, updatedAt, postId, userId)
    pure $ fromOnly <$> listToMaybe ids

  delete :: Id Comment -> App ()
  delete commentId = do
    let sql = "DELETE FROM comments WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    void $ liftIO $ withResource pool $ \conn -> execute conn sql [commentId]

  findByPost :: Id Post -> Sorting -> Maybe Cursor -> Int -> App [Entity Comment]
  findByPost idPost sorting maybeCursor count = do
    let
      sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
          \  FROM comments \
          \  WHERE post_id = ? " <> andCursor maybeCursor <> " \
          \  ORDER BY " <> sortExpression sorting <> " \
          \  FETCH FIRST ? ROWS ONLY"
      andCursor = maybe mempty (\c -> "AND " <> cursorExpression c)
    pool <- asks (connectionPool . databaseContext)
    liftIO $ withResource pool $ \conn -> query conn sql (idPost, count)
