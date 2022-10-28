{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Api (api, apiServer) where

import qualified Controllers.CommentController as CommentController
import qualified Controllers.PostController as PostController
import Data.Data (Proxy(Proxy))
import Servant (Server)
import Servant.API (type (:<|>)(..))

type Controllers = PostController.Routes :<|> CommentController.Routes

apiServer :: Server Controllers
apiServer = PostController.handlers :<|> CommentController.handlers

api :: Proxy Controllers
api = Proxy
