{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Api (api, server) where

import App (App)
import qualified Controllers.AuthController as AuthController
import qualified Controllers.CommentController as CommentController
import qualified Controllers.PostController as PostController
import Data.Data (Proxy(Proxy))
import Models.User (User(..))
import Servant (ServerT, err401, type (:<|>)(..), type (:>))
import qualified Servant.Auth.Server as Sas

type SecuredRoutes = PostController.Routes :<|> CommentController.Routes

securedHandlers :: Sas.AuthResult User -> ServerT SecuredRoutes App
securedHandlers (Sas.Authenticated _)
  = PostController.handlers :<|> CommentController.handlers
securedHandlers _ = Sas.throwAll err401

type Api auths = Sas.Auth auths User :> SecuredRoutes :<|> AuthController.Login

server :: Sas.CookieSettings -> Sas.JWTSettings -> ServerT (Api auths) App
server cs jwts = securedHandlers :<|> AuthController.login cs jwts

api :: Proxy (Api '[Sas.JWT])
api = Proxy
