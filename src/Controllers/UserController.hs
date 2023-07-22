{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Controllers.UserController (getUser, getCurrentUser, createUser) where

import Control.Category ((>>>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import qualified Controllers.Types.Error as Error
import qualified Data.Password.Bcrypt as Bcrypt
import Dto.UserDto (NewUserDto(..), UserDto, UserIdDto(..))
import qualified Dto.UserDto as UserDto
import Models.Types.Id (Id, unwrap)
import Models.User (User(..))
import RequestContext (RequestContext(..))
import qualified Stores.UserStore as UserStore
import Stores.UserStore (UserStore)

getUser :: (MonadThrow m, UserStore m) => Id User -> m UserDto
getUser userId = UserStore.find userId >>= \case
  Just user -> pure (UserDto.fromEntity user)
  Nothing -> throwM (Error.notFound "Could not find user with such ID.")

getCurrentUser :: (?requestCtx :: RequestContext)
  => (MonadThrow m, UserStore m)
  => m UserDto
getCurrentUser = getUser (RequestContext.userId ?requestCtx)

createUser :: (MonadThrow m, UserStore m, MonadIO m) => NewUserDto -> m UserIdDto
createUser NewUserDto {..} = UserStore.findByUsername username >>= \case
  Nothing -> do
    hashed <- hash password
    UserStore.save (User username email) hashed >>= \case
      Just userId -> pure $ UserIdDto (unwrap userId)
      Nothing -> throwM (Error.serverError "Failed to create user.")
  Just _ -> throwM (Error.badRequest "Username already in use.")
  where
    hash = Bcrypt.mkPassword >>> Bcrypt.hashPassword >>> fmap Bcrypt.unPasswordHash
