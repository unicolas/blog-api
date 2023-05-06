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
import Models.Types.Aggregate (Aggregate)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.User (User(..))
import qualified Stores.Query as Query

class Monad m => UserStore m where
  find :: Id User -> m (Maybe (Entity User))
  save :: User -> m (Maybe (Id User))
  findWithCredentials :: Text -> m (Maybe (Aggregate User Credentials))

instance UserStore App where
  find :: Id User -> App (Maybe (Entity User))
  find idUser = do
    pool <- asks (connectionPool . databaseContext)
    users <- Query.fetch
        [ "SELECT id, username, email, user_id, password"
        , "FROM users"
        , "WHERE id = ?"
        ] [idUser]
      & withResource pool
      & liftIO
    pure (listToMaybe users)

  save :: User -> App (Maybe (Id User))
  save user = do
    pool <- asks (connectionPool . databaseContext)
    ids <- Query.fetch
        [ "INSERT INTO users (id, username, email)"
        , "VALUES (gen_random_uuid(), ?, ?)"
        , "RETURNING id"
        ] (username user, email user)
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
