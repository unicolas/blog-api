{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stores.PostStore (PostStore (..)) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(fromOnly), execute, query, query_)
import Models.Post (Post(..))
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.User (User)
import Stores.Types.Database (Database, connection)

class Monad m => PostStore m where
  find :: Id Post -> m (Maybe (Entity Post))
  findAll :: m [Entity Post]
  save :: Post -> m (Maybe (Id Post))
  delete :: Id Post -> m ()
  findByAuthor :: Id User -> m [Entity Post]

instance PostStore Database where
  find :: Id Post -> Database (Maybe (Entity Post))
  find idPost = liftIO $ do
    let sql = "SELECT id, title, content, user_id, created_at, updated_at \
            \  FROM posts \
            \  WHERE id = ?"
    conn <- connection
    posts <- query conn sql [idPost]
    pure $ listToMaybe posts

  findAll :: Database [Entity Post]
  findAll = liftIO $ do
    let sql = "SELECT id, title, content, user_id, created_at, updated_at \
            \  FROM posts"
    conn <- connection
    query_ conn sql

  save :: Post -> Database (Maybe (Id Post))
  save post = liftIO $ do
    let sql = "INSERT INTO posts (id, title, content, created_at, updated_at, user_id) \
            \  VALUES (gen_random_uuid(), ?, ?, now(), now(), ?) \
            \  RETURNING id"
    conn <- connection
    ids <- query conn sql (title post, content post, userId post)
    pure $ listToMaybe $ fromOnly <$> ids

  delete :: Id Post -> Database ()
  delete idPost = liftIO $ do
    let sql = "DELETE FROM posts WHERE id = ?"
    conn <- connection
    void $ execute conn sql [idPost]

  findByAuthor :: Id User -> Database [Entity Post]
  findByAuthor idAuthor = liftIO $ do
    let sql = "SELECT id, title, content, user_id, created_at, updated_at \
            \  FROM posts \
            \  WHERE user_id = ?"
    conn <- connection
    query conn sql [idAuthor]
