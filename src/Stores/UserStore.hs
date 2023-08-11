{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Stores.UserStore (UserStore(..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple (fromOnly)
import DatabaseContext (DatabaseContext(..))
import Models.Credentials (Credentials)
import Models.HashedPassword (HashedPassword)
import Models.Types.Aggregate (Aggregate)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.User (User(..))
import Models.Username (Username)
import qualified Stores.Query as Query

class Monad m => UserStore m where
  find :: Id User -> m (Maybe (Entity User))
  save :: User -> HashedPassword -> m (Maybe (Id User))
  findWithCredentials :: Text -> m (Maybe (Aggregate User Credentials))
  findByUsername :: Username -> m (Maybe (Entity User))

instance UserStore App where
  find :: Id User -> App (Maybe (Entity User))
  find idUser = do
    pool <- asks (connectionPool . databaseContext)
    users <- Query.fetch
        ["SELECT id, username, email FROM users WHERE id = ?"] [idUser]
      & withResource pool
      & liftIO
    pure (listToMaybe users)

  save :: User -> HashedPassword -> App (Maybe (Id User))
  save user psw = do
    pool <- asks (connectionPool . databaseContext)
    ids <- Query.fetch
        [ "WITH newUser AS ("
        , "  INSERT INTO users (id, username, email)"
        , "  VALUES (gen_random_uuid(), ?, ?)"
        , "  RETURNING id )"
        , "INSERT INTO user_credentials"
        , "VALUES ((SELECT id AS user_id FROM newUser), ?)"
        , "RETURNING user_id"
        ] (username user, email user, psw)
      & withResource pool
      & liftIO
    pure (fromOnly <$> listToMaybe ids)

  findWithCredentials :: Text -> App (Maybe (Aggregate User Credentials))
  findWithCredentials aUsername = do
    pool <- asks (connectionPool . databaseContext)
    users <- Query.fetch
        [ "SELECT id, username, email, user_id, password"
        , "FROM users"
        , "INNER JOIN user_credentials ON id = user_id"
        , "WHERE username = ?"
        ] [aUsername]
      & withResource pool
      & liftIO
    pure (listToMaybe users)

  findByUsername :: Username -> App (Maybe (Entity User))
  findByUsername aUsername = do
    pool <- asks (connectionPool . databaseContext)
    users <- Query.fetch
        ["SELECT id, username, email FROM users WHERE username = ?"] [aUsername]
      & withResource pool
      & liftIO
    pure (listToMaybe users)
