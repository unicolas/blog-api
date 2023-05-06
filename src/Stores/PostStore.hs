{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stores.PostStore (PostStore (..)) where

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
import Models.Post (Post(..))
import Models.Types.Cursor (Cursor, cursorExpression)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.Types.Sorting (Sorting, sortExpression)
import Models.User (User)
import qualified Stores.Query as Query

class Monad m => PostStore m where
  find :: Id Post -> m (Maybe (Entity Post))
  findAll :: Sorting -> Maybe Cursor -> Int -> m [Entity Post]
  save :: Post -> m (Maybe (Id Post))
  delete :: Id Post -> m ()
  findByAuthor :: Id User -> Sorting -> Maybe Cursor -> Int -> m [Entity Post]

instance PostStore App where
  find :: Id Post -> App (Maybe (Entity Post))
  find idPost = do
    pool <- asks (connectionPool . databaseContext)
    posts <- Query.fetch
        [ "SELECT id, title, content, user_id, created_at, updated_at"
        , "FROM posts"
        , "WHERE id = ?"
        ] [idPost]
      & withResource pool
      & liftIO
    pure (listToMaybe posts)

  findAll :: Sorting -> Maybe Cursor -> Int -> App [Entity Post]
  findAll sorting maybeCursor count = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        [ "SELECT id, title, content, user_id, created_at, updated_at"
        , "FROM posts"
        , maybe mempty (("WHERE " <>) . cursorExpression) maybeCursor
        , "ORDER BY", sortExpression sorting
        , "FETCH FIRST ? ROWS ONLY"
        ] [count]
      & withResource pool
      & liftIO

  save :: Post -> App (Maybe (Id Post))
  save Post{..} = do
    pool <- asks (connectionPool . databaseContext)
    ids <- Query.fetch
        [ "INSERT INTO posts (id, title, content, created_at, updated_at, user_id)"
        , "VALUES (gen_random_uuid(), ?, ?, ?, ?, ?)"
        , "RETURNING id"
        ] (title, content, createdAt, updatedAt, userId)
      & withResource pool
      & liftIO
    pure (fromOnly <$> listToMaybe ids)

  delete :: Id Post -> App ()
  delete idPost = do
    pool <- asks (connectionPool . databaseContext)
    Query.run ["DELETE FROM posts WHERE id = ?"] [idPost]
      & withResource pool
      & liftIO
      & void

  findByAuthor :: Id User -> Sorting -> Maybe Cursor -> Int -> App [Entity Post]
  findByAuthor idAuthor sorting maybeCursor count = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        [ "SELECT id, title, content, user_id, created_at, updated_at"
        , "FROM posts"
        , "WHERE user_id = ?"
        , maybe mempty (("AND " <>) . cursorExpression) maybeCursor
        , "ORDER BY", sortExpression sorting
        , "FETCH FIRST ? ROWS ONLY"
        ] (idAuthor, count)
      & withResource pool
      & liftIO
