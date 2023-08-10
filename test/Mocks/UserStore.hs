{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.UserStore (UserStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock
import Models.Credentials (Credentials(..))
import Models.Types.Aggregate (Aggregate(Aggregate))
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.User (User(..))
import Stores.UserStore (UserStore(..))
import Models.Username (Username(Username))

instance UserStore AppMock where
  find :: Id User -> AppMock (Maybe (Entity User))
  find idUser = gets (Map.lookup idUser . AppMock.users)

  findWithCredentials :: Text -> AppMock (Maybe (Aggregate User Credentials))
  findWithCredentials aUsername = do
    maybeUser <- gets (List.find withUsername . Map.elems . AppMock.users)
    maybeCreds <- case maybeUser of
      Nothing -> pure Nothing
      Just user -> gets (Map.lookup (getId user) . AppMock.credentials)
    pure (Aggregate <$> maybeUser <*> maybeCreds)
    where
      withUsername (Entity _ user) = aUsername == getUsername user
      getId (Entity i _) = i
      getUsername (User {username = (Username u)}) = u

  save :: User -> Text -> AppMock (Maybe (Id User))
  save user psw = do
    idUser <- liftIO (Id <$> nextRandom)
    users <- gets (Map.insert idUser (Entity idUser user) . AppMock.users)
    credentials <- gets
      $ Map.insert idUser (Credentials idUser psw) . AppMock.credentials
    modify (\s -> s {AppMock.users = users, AppMock.credentials = credentials})
    pure (Just idUser)

  findByUsername :: Username -> AppMock (Maybe (Entity User))
  findByUsername username' = gets
    $ List.find (\(Entity _ user) -> username user == username')
    . Map.elems
    . AppMock.users
