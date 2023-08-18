{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controllers.AuthController (login, LoginRequest(..), LoginResponse(..)) where

import Auth (signToken)
import AuthClaims (accessClaims, refreshClaims)
import Control.Monad (liftM2, when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Crypto.JOSE (JWK)
import qualified Crypto.JWT as Jwt
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import qualified Data.Password.Bcrypt as Bcrypt
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Models.Credentials (Credentials(..))
import Models.HashedPassword (HashedPassword(HashedPassword))
import Models.Types.Aggregate (Aggregate(Aggregate))
import Models.Types.Entity (Entity(..))
import Models.Username (makeUsername)
import qualified Stores.UserStore as UserStore
import Stores.UserStore (UserStore)

data LoginRequest = LoginRequest
  { username :: !Text
  , password :: !Text
  }
  deriving (Generic, FromJSON)

data LoginResponse = LoginResponse
  { access :: !String
  , refresh :: !String
  }
  deriving (Generic, ToJSON)

login :: (MonadThrow m, UserStore m, MonadIO m)
  => JWK -> LoginRequest -> m LoginResponse
login jwk LoginRequest {..} = do
  let parsedUsername = maybeRight (makeUsername username)
  maybeAggr <- maybe (pure Nothing) UserStore.findWithCredentials parsedUsername
  (Entity userId _, creds) <- case maybeAggr of
    Nothing -> throwM Error.unauthorized
    Just (Aggregate u c) -> pure (u, c)
  when (Bcrypt.PasswordCheckFail == checkPassword password creds)
    $ throwM Error.unauthorized
  now <- liftIO getCurrentTime
  maybeJwts <- liftIO $ sequenceTuple
    ( signToken jwk (accessClaims userId now)
    , signToken jwk (refreshClaims userId now)
    )
  case maybeJwts of
    (Just (toString -> access), Just (toString -> refresh))
      -> pure LoginResponse {..}
    _ -> throwM Error.unauthorized
  where
    sequenceTuple = uncurry (liftM2 (,))
    maybeRight = either (const Nothing) Just
    checkPassword plain Credentials {password = HashedPassword pswd} =
      Bcrypt.checkPassword (Bcrypt.mkPassword plain) (Bcrypt.PasswordHash pswd)
    toString = LazyByteString.toString . Jwt.encodeCompact

