{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Controllers.AuthController (LoginHeaders, login, LoginRequest(..)) where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Data.Aeson (FromJSON)
import qualified Data.Password.Bcrypt as Bcrypt
import Data.Text (Text)
import GHC.Generics (Generic)
import Models.Credentials (Credentials(..))
import qualified Models.Credentials as Credentials
import Models.HashedPassword (HashedPassword(HashedPassword))
import Models.Types.Aggregate (Aggregate(Aggregate))
import qualified Servant as Http (Header, Headers, NoContent(..))
import qualified Servant.Auth.Server as Sas
import qualified Stores.UserStore as UserStore
import Stores.UserStore (UserStore)

data LoginRequest = LoginRequest
  { username :: !Text
  , password :: !Text
  }
  deriving (Generic, FromJSON)

type LoginHeaders = Http.Headers
  '[ Http.Header "Set-Cookie" Sas.SetCookie
   , Http.Header "Set-Cookie" Sas.SetCookie ]
  Http.NoContent

login :: (MonadThrow m, UserStore m, MonadIO m)
  => Sas.CookieSettings -> Sas.JWTSettings -> LoginRequest -> m LoginHeaders
login cookieSettings jwtSettings (LoginRequest reqUser reqPassword) = do
  maybeAggr <- UserStore.findWithCredentials reqUser
  (user, creds) <- case maybeAggr of
    Nothing -> throwM Error.unauthorized
    Just (Aggregate u c) -> pure (u, c)
  when (Bcrypt.PasswordCheckFail == checkPassword reqPassword creds)
    $ throwM Error.unauthorized
  maybeCookies <- liftIO (Sas.acceptLogin cookieSettings jwtSettings user)
  case maybeCookies of
    Nothing -> throwM Error.unauthorized
    Just cookies -> pure (cookies Http.NoContent)

checkPassword :: Text -> Credentials -> Bcrypt.PasswordCheck
checkPassword plain creds = Bcrypt.checkPassword
  (Bcrypt.mkPassword plain)
  (Bcrypt.PasswordHash pswd)
  where
    HashedPassword pswd = Credentials.password creds
