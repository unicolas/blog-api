{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.UserController (getUser) where

import Control.Monad.Catch (MonadThrow, throwM)
import qualified Controllers.Types.Error as Error
import Dto.UserDto (UserDto)
import qualified Dto.UserDto as UserDto
import Models.Types.Id (Id)
import Models.User (User)
import qualified Stores.UserStore as UserStore
import Stores.UserStore (UserStore)

getUser :: (MonadThrow m, UserStore m) => Id User -> m UserDto
getUser userId = UserStore.find userId >>= \case
  Just user -> pure (UserDto.fromEntity user)
  Nothing -> throwM (Error.notFound "Could not find user with such ID.")