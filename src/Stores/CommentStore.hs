{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Stores.CommentStore (CommentStore (..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute, fromOnly, query, query_)
import DatabaseContext (DatabaseContext(..))
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)

class Monad m => CommentStore m where
  find :: Id Comment -> m (Maybe (Entity Comment))
  findAll :: m [Entity Comment]
  save :: Comment -> m (Maybe (Id Comment))
  delete :: Id Comment -> m ()
  findByPost :: Id Post -> m [Entity Comment]

instance CommentStore App where
  find :: Id Comment -> App (Maybe (Entity Comment))
  find idComment = do
    let sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
            \  FROM comments \
            \  WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    comments <- liftIO $ withResource pool $ \conn -> query conn sql [idComment]
    pure $ listToMaybe comments

  findAll :: App [Entity Comment]
  findAll = do
    let sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
            \  FROM comments"
    pool <- asks $ connectionPool . databaseContext
    liftIO $ withResource pool (`query_` sql)

  save :: Comment -> App (Maybe (Id Comment))
  save comment = do
    let sql = "INSERT INTO comments \
            \  (id, title, content, created_at, updated_at, post_id, user_id) \
            \  VALUES (gen_random_uuid(), ?, ?, now(), now(), ?, ?) \
            \  RETURNING id"
    pool <- asks $ connectionPool . databaseContext
    ids <- liftIO $ withResource pool
      $ \conn -> query conn sql (title comment, content comment, postId comment, userId comment)
    pure $ fromOnly <$> listToMaybe ids

  delete :: Id Comment -> App ()
  delete commentId = do
    let sql = "DELETE FROM comments WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    void $ liftIO $ withResource pool $ \conn -> execute conn sql [commentId]

  findByPost :: Id Post -> App [Entity Comment]
  findByPost idPost = do
    let sql = "SELECT * FROM comments WHERE post_id = ?"
    pool <- asks $ connectionPool . databaseContext
    liftIO $ withResource pool $ \conn -> query conn sql [idPost]
