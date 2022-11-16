{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.AuthController (Login, login) where

import App (App)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import qualified Data.Password.Bcrypt as Bcrypt
import Data.Text (Text)
import GHC.Generics (Generic)
import Models.Credentials (Credentials)
import qualified Models.Credentials as Credentials
import Models.Types.Aggregate (deconstruct)
import qualified Servant as Http (Header, Headers, NoContent(..), Post)
import Servant (JSON, ReqBody, ServerT, err401, type (:>))
import qualified Servant.Auth.Server as Sas
import qualified Stores.UserStore as UserStore

data LoginRequest = LoginRequest
  { username :: !Text
  , password :: !Text
  }
  deriving (Show, Generic, FromJSON)

type LoginHeaders = Http.Headers
  '[ Http.Header "Set-Cookie" Sas.SetCookie
   , Http.Header "Set-Cookie" Sas.SetCookie ]
  Http.NoContent

-- POST /login
type Login = "login"
  :> ReqBody '[JSON] LoginRequest
  :> Http.Post '[JSON] LoginHeaders

login :: Sas.CookieSettings -> Sas.JWTSettings -> ServerT Login App
login = checkCreds

checkCreds :: Sas.CookieSettings -> Sas.JWTSettings -> LoginRequest -> App LoginHeaders
checkCreds cookieSettings jwtSettings (LoginRequest reqUser reqPassword) = do
  maybeAggr <- UserStore.findWithCredentials reqUser
  (user, creds) <- case maybeAggr of
    Nothing -> throwM err401
    Just aggregate -> pure $ deconstruct aggregate
  when (Bcrypt.PasswordCheckFail == checkPassword reqPassword creds)
    $ throwM err401
  maybeCookies <- liftIO $ Sas.acceptLogin cookieSettings jwtSettings user
  case maybeCookies of
    Nothing -> throwM err401
    Just cookies -> pure $ cookies Http.NoContent

checkPassword :: Text -> Credentials -> Bcrypt.PasswordCheck
checkPassword plain creds = Bcrypt.checkPassword
  (Bcrypt.mkPassword plain)
  (Bcrypt.PasswordHash (Credentials.password creds))
