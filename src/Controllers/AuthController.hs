{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Controllers.AuthController (login, LoginRequest(..), LoginResponse(..), refreshToken) where

import Auth (signToken)
import AuthClaims
  (RefreshClaims, accessClaims, expireAtClaim, refreshClaims, subjectClaim)
import Control.Arrow ((&&&))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Crypto.JOSE (JWK)
import Crypto.JWT (SignedJWT)
import qualified Crypto.JWT as Jwt
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import Data.ByteString.UTF8 (ByteString)
import qualified Data.Password.Bcrypt as Bcrypt
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Models.Credentials (Credentials(..))
import Models.HashedPassword (HashedPassword(HashedPassword))
import Models.Types.Aggregate (Aggregate(Aggregate))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import Models.User (User)
import Models.Username (makeUsername)
import Stores.TokenStore (TokenStore(blacklist))
import qualified Stores.UserStore as UserStore
import Stores.UserStore (UserStore)
import Utility (maybeRight)

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
login jwk LoginRequest {username, password} = do
  let parsedUsername = maybeRight (makeUsername username)
  maybeAggr <- maybe (pure Nothing) UserStore.findWithCredentials parsedUsername
  (Entity userId _, creds) <- case maybeAggr of
    Nothing -> throwM Error.unauthorized
    Just (Aggregate u c) -> pure (u, c)
  when (Bcrypt.PasswordCheckFail == checkPassword password creds)
    $ throwM Error.unauthorized
  newTokens <- signNewTokens jwk userId
  makeLoginResponse newTokens
  where
    checkPassword plain Credentials {password = HashedPassword pswd} =
      Bcrypt.checkPassword (Bcrypt.mkPassword plain) (Bcrypt.PasswordHash pswd)

refreshToken :: (MonadThrow m, MonadIO m, TokenStore m)
  => JWK -> Maybe (RefreshClaims, ByteString) -> m LoginResponse
refreshToken jwk = \case
  Just (subjectClaim &&& expireAtClaim -> (Just userId, Just expAt), token)
    -> (signNewTokens jwk userId >>= makeLoginResponse) <* blacklist token expAt
  _ -> throwM Error.unauthorized

makeLoginResponse :: MonadThrow m => [Maybe SignedJWT] -> m LoginResponse
makeLoginResponse = \case
  [Just (toString -> access), Just (toString -> refresh)]
    -> pure LoginResponse {access, refresh}
  _ -> throwM (Error.serverError "Failed to generate new tokens")
  where
    toString = LazyByteString.toString . Jwt.encodeCompact

signNewTokens :: MonadIO m => JWK -> Id User -> m [Maybe SignedJWT]
signNewTokens jwk userId = liftIO $ do
  now <- getCurrentTime
  sequence
    [ signToken jwk (accessClaims userId now)
    , signToken jwk (refreshClaims userId now)
    ]
