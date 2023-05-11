{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stores.PostStore (PostStore (..), findWithTags) where

import App (App)
import AppContext (AppContext(..))
import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (fromOnly)
import DatabaseContext (DatabaseContext(..))
import Models.Post (Post(..))
import Models.Tag (Tag)
import Models.Types.Aggregate (Aggregate(..), aggregateMaybe)
import Models.Types.Cursor (cursorExpression)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.Types.Pagination (Pagination(..))
import Models.Types.Sorting (sortExpression)
import Models.User (User)
import qualified Stores.Query as Query
import qualified Stores.TagStore as TagStore
import Stores.TagStore (TagStore)

class Monad m => PostStore m where
  find :: Id Post -> m (Maybe (Entity Post))
  findAll :: Pagination -> m [Entity Post]
  save :: Post -> m (Maybe (Id Post))
  delete :: Id Post -> m ()
  findByAuthor :: Id User -> Pagination -> m [Entity Post]

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

  findAll :: Pagination -> App [Entity Post]
  findAll Pagination {..} = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        [ "SELECT id, title, content, user_id, created_at, updated_at"
        , "FROM posts"
        , maybe mempty (("WHERE " <>) . cursorExpression) cursor
        , "ORDER BY", sortExpression (sort, order)
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

  findByAuthor :: Id User -> Pagination -> App [Entity Post]
  findByAuthor idAuthor Pagination {..} = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        [ "SELECT id, title, content, user_id, created_at, updated_at"
        , "FROM posts"
        , "WHERE user_id = ?"
        , maybe mempty (("AND " <>) . cursorExpression) cursor
        , "ORDER BY", sortExpression (sort, order)
        , "FETCH FIRST ? ROWS ONLY"
        ] (idAuthor, count)
      & withResource pool
      & liftIO

findWithTags :: (PostStore m, TagStore m)
  => Id Post
  -> m (Maybe (Aggregate Post [Tag]))
findWithTags = aggregateMaybe . (find &&& TagStore.findByPost)
