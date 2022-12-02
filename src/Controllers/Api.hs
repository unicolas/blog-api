{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Api (api, server) where

import App (App)
import qualified Controllers.AuthController as AuthController
import qualified Controllers.CommentController as CommentController
import qualified Controllers.PostController as PostController
import qualified Controllers.Types.Error as Error
import Data.Data (Proxy(Proxy))
import Models.Types.Entity (Entity)
import Models.User (User(..))
import qualified RequestContext
import Servant (ServerT, type (:<|>)(..), type (:>))
import qualified Servant.Auth.Server as Sas

type SecuredRoutes = PostController.Routes :<|> CommentController.Routes

securedHandlers :: Sas.AuthResult (Entity User) -> ServerT SecuredRoutes App
securedHandlers = \case
  Sas.Authenticated user ->
    let ?requestCtx = RequestContext.make user
    in PostController.handlers :<|> CommentController.handlers
  _ -> Sas.throwAll Error.unauthorized

type Api auths = Sas.Auth auths (Entity User) :> SecuredRoutes :<|> AuthController.Login

server :: Sas.CookieSettings -> Sas.JWTSettings -> ServerT (Api auths) App
server cs jwts = securedHandlers :<|> AuthController.login cs jwts

api :: Proxy (Api '[Sas.JWT])
api = Proxy
