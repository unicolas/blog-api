{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.UserController (getUser, getCurrentUser, createUser) where

import Control.Category ((>>>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import qualified Controllers.Types.Error as Error
import qualified Data.Password.Bcrypt as Bcrypt
import Dto.UserDto (NewUserDto(..), UserDto, UserIdDto(..))
import qualified Dto.UserDto as NewUserDto (NewUserDto(..))
import qualified Dto.UserDto as UserDto
import Models.Email (Email(..))
import Models.Password (Password(..))
import Models.Types.Id (Id(..))
import Models.User (User(..))
import Models.Username (Username(..))
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
createUser dto = UserStore.findByUsername username >>= \case
  Nothing -> do
    hashed <- hash password
    UserStore.save (User username email) hashed >>= \case
      Just (Id userId) -> pure (UserIdDto userId)
      Nothing -> throwM (Error.serverError "Failed to create user.")
  Just _ -> throwM (Error.badRequest "Username already in use.")
  where
    (Username username) = NewUserDto.username dto
    (Email email) = NewUserDto.email dto
    (Password password) = NewUserDto.password dto
    hash = Bcrypt.mkPassword >>> Bcrypt.hashPassword >>> fmap Bcrypt.unPasswordHash
