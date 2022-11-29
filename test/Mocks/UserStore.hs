{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.UserStore (UserStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Mocks.StorageMock (StorageMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Credentials (Credentials(..))
import Models.Types.Aggregate (Aggregate(Aggregate))
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Models.User (User(..))
import Stores.UserStore (UserStore(..))

instance UserStore StorageMock where
  find :: Id User -> StorageMock (Maybe (Entity User))
  find idUser = gets (Map.lookup idUser . StorageMock.users)

  save :: User -> StorageMock (Maybe (Id User))
  save user = do
    idUser <- liftIO (Id <$> nextRandom)
    users <- gets (Map.insert idUser (Entity idUser user) . StorageMock.users)
    modify (\s -> s {StorageMock.users = users})
    pure $ Just idUser

  findWithCredentials :: Text -> StorageMock (Maybe (Aggregate User Credentials))
  findWithCredentials aUsername = do
    maybeUser <- gets (List.find withUsername . Map.elems . StorageMock.users)
    maybeCreds <- case maybeUser of
      Nothing -> pure Nothing
      Just user -> gets (Map.lookup (getId user) . StorageMock.credentials)
    pure (Aggregate <$> maybeUser <*> maybeCreds)
    where
      withUsername (Entity _ user) = aUsername == username user
      getId (Entity i _) = i
