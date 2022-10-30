{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stores.CommentStore (CommentStore (..)) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (execute, fromOnly, query, query_)
import Models.Comment (Comment(..))
import Models.Post (Post)
import Stores.Types.Database (Database, connection)
import Stores.Types.Entity (Entity)
import Stores.Types.Id (Id)

class Monad m => CommentStore m where
  find :: Id Comment -> m (Maybe (Entity Comment))
  findAll :: m [Entity Comment]
  save :: Comment -> m (Maybe (Id Comment))
  delete :: Id Comment -> m ()
  findByPost :: Id Post -> m [Entity Comment]

instance CommentStore Database where
  find :: Id Comment -> Database (Maybe (Entity Comment))
  find idComment = liftIO $ do
    let sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
            \  FROM comments \
            \  WHERE id = ?"
    conn <- connection
    comments <- query conn sql [idComment]
    pure $ listToMaybe comments

  findAll :: Database [Entity Comment]
  findAll = liftIO $ do
    let sql = "SELECT id, title, content, created_at, updated_at, post_id, user_id \
            \  FROM comments"
    conn <- connection
    query_ conn sql

  save :: Comment -> Database (Maybe (Id Comment))
  save comment = liftIO $ do
    let sql = "INSERT INTO comments \
            \  (id, title, content, created_at, updated_at, post_id, user_id) \
            \  VALUES (gen_random_uuid(), ?, ?, now(), now(), ?, ?) \
            \  RETURNING id"
    conn <- connection
    ids <- query conn sql (title comment, content comment, postId comment, userId comment)
    pure $ listToMaybe $ fromOnly <$> ids

  delete :: Id Comment -> Database ()
  delete commentId = liftIO $ do
    let sql = "DELETE FROM comments WHERE id = ?"
    conn <- connection
    void $ execute conn sql [commentId]

  findByPost :: Id Post -> Database [Entity Comment]
  findByPost idPost = liftIO $ do
    let sql = "SELECT * FROM comments WHERE post_id = ?"
    conn <- connection
    query conn sql [idPost]
