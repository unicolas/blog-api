{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Stores.UserStore (UserStore(..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple (execute, fromOnly, query)
import DatabaseContext (DatabaseContext(..))
import Models.Credentials (Credentials)
import Models.Types.Aggregate (Aggregate)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.User (User(..))

class Monad m => UserStore m where
  find :: Id User -> m (Maybe (Entity User))
  save :: User -> m (Maybe (Id User))
  delete :: Id User -> m ()
  findWithCredentials :: Text -> m (Maybe (Aggregate User Credentials))

instance UserStore App where
  find :: Id User -> App (Maybe (Entity User))
  find idUser = do
    let sql = "SELECT * FROM users WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    users <- liftIO $ withResource pool $ \conn -> query conn sql [idUser]
    pure $ listToMaybe users

  save :: User -> App (Maybe (Id User))
  save user = do
    let sql = "INSERT INTO users (id, username, email) \
            \  VALUES (gen_random_uuid(), ?, ?) \
            \  RETURNING id"
    pool <- asks $ connectionPool . databaseContext
    ids <- liftIO $ withResource pool
      $ \conn -> query conn sql (username user, email user)
    pure $ listToMaybe $ fromOnly <$> ids

  delete :: Id User -> App ()
  delete idUser = do
    let sql = "DELETE FROM users WHERE id = ?"
    pool <- asks $ connectionPool . databaseContext
    void $ liftIO $ withResource pool $ \conn -> execute conn sql [idUser]

  findWithCredentials :: Text -> App (Maybe (Aggregate User Credentials))
  findWithCredentials aUsername = do
    let sql = "SELECT id, username, email, user_id, password \
            \ FROM users \
            \ INNER JOIN user_credentials ON id = user_id \
            \ WHERE username = ?"
    pool <- asks $ connectionPool . databaseContext
    users <- liftIO $ withResource pool $ \conn -> query conn sql [aUsername]
    pure $ listToMaybe users
