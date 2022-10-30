{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stores.UserStore (UserStore(..)) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (execute, fromOnly, query)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.User (User(..))
import Stores.Types.Database (Database, connection)

class Monad m => UserStore m where
  find :: Id User -> m (Maybe (Entity User))
  save :: User -> m (Maybe (Id User))
  delete :: Id User -> m ()

instance UserStore Database where
  find :: Id User -> Database (Maybe (Entity User))
  find idUser = liftIO $ do
    let sql = "SELECT * FROM users WHERE id = ?"
    conn <- connection
    users <- query conn sql [idUser]
    pure $ listToMaybe users

  save :: User -> Database (Maybe (Id User))
  save user = liftIO $ do
    let sql = "INSERT INTO users (id, username, email) \
            \  VALUES (gen_random_uuid(), ?, ?) \
            \  RETURNING id"
    conn <- connection
    ids <- query conn sql (username user, email user)
    pure $ listToMaybe $ fromOnly <$> ids

  delete :: Id User -> Database ()
  delete idUser = liftIO $ do
    let sql = "DELETE FROM users WHERE id = ?"
    conn <- connection
    void $ execute conn sql [idUser]
