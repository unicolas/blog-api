{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stores.PostStore (PostStore (..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Only(fromOnly), execute, query, query_)
import DatabaseContext (DatabaseContext(..))
import Models.Post (Post(..))
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.Types.Sorting (Sorting, sortExpression)
import Models.User (User)

class Monad m => PostStore m where
  find :: Id Post -> m (Maybe (Entity Post))
  findAll :: Sorting -> m [Entity Post]
  save :: Post -> m (Maybe (Id Post))
  delete :: Id Post -> m ()
  findByAuthor :: Id User -> Sorting -> m [Entity Post]

instance PostStore App where
  find :: Id Post -> App (Maybe (Entity Post))
  find idPost = do
    let sql = "SELECT id, title, content, user_id, created_at, updated_at \
            \  FROM posts \
            \  WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    posts <- liftIO $ withResource pool $ \conn -> query conn sql [idPost]
    pure $ listToMaybe posts

  findAll :: Sorting -> App [Entity Post]
  findAll sorting = do
    let sql = "SELECT id, title, content, user_id, created_at, updated_at \
            \  FROM posts \
            \  ORDER BY " <> sortExpression sorting
    pool <- asks $ connectionPool . databaseContext
    liftIO $ withResource pool (`query_` sql)

  save :: Post -> App (Maybe (Id Post))
  save Post{..} = do
    let sql = "INSERT INTO posts (id, title, content, created_at, updated_at, user_id) \
            \  VALUES (gen_random_uuid(), ?, ?, ?, ?, ?) \
            \  RETURNING id"
    pool <- asks $ connectionPool . databaseContext
    ids <- liftIO $ withResource pool
      $ \conn -> query conn sql (title, content, createdAt, updatedAt, userId)
    pure $ fromOnly <$> listToMaybe ids

  delete :: Id Post -> App ()
  delete idPost = do
    let sql = "DELETE FROM posts WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    void $ liftIO $ withResource pool $ \conn -> execute conn sql [idPost]

  findByAuthor :: Id User -> Sorting -> App [Entity Post]
  findByAuthor idAuthor sorting = do
    let sql = "SELECT id, title, content, user_id, created_at, updated_at \
            \  FROM posts \
            \  WHERE user_id = ? \
            \  ORDER BY " <> sortExpression sorting
    pool <- asks $ connectionPool . databaseContext
    liftIO $ withResource pool $ \conn -> query conn sql [idAuthor]
