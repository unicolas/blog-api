{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Api (handlers) where

import App (App)
import Controllers.AuthController (LoginHeaders, LoginRequest)
import qualified Controllers.AuthController as AuthController
import qualified Controllers.CommentController as CommentController
import qualified Controllers.PostController as PostController
import qualified Controllers.Types.Error as Error
import Dto.CommentDto (CommentDto, CommentIdDto, NewCommentDto)
import Dto.Page (Page)
import Dto.PostDto (NewPostDto, PostDto, PostIdDto)
import GHC.Generics (Generic)
import Models.Comment (Comment)
import Models.Post (Post)
import Models.Types.Cursor (Cursor)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.Types.Sorting (Order, Sort)
import Models.User (User(..))
import qualified RequestContext
import RequestContext (RequestContext)
import SasOrphans ()
import qualified Servant as Http (Delete, Get, NoContent(..), Post)
import Servant (Capture, JSON, NamedRoutes, QueryParam, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import qualified Servant.Auth.Server as Sas
import Servant.Server.Generic (AsServerT)

data Api mode = Api
  { login :: mode
      -- POST /login
      :- "login"
      :> ReqBody '[JSON] LoginRequest
      :> Http.Post '[JSON] LoginHeaders
  , secured :: mode
      :- Sas.Auth '[Sas.JWT] (Entity User)
      :> NamedRoutes SecuredRoutes
  }
  deriving Generic

handlers :: Sas.CookieSettings -> Sas.JWTSettings -> Api (AsServerT App)
handlers cs jwts = Api
  { login = AuthController.login cs jwts
  , secured = securedHandlers
  }

data SecuredRoutes mode = SecuredRoutes
  { posts :: mode
      :- "posts"
      :> NamedRoutes PostRoutes
  , comments :: mode
      :- "comments"
      :> NamedRoutes CommentRoutes
  }
  deriving (Generic)

securedHandlers :: Sas.AuthResult (Entity User) -> SecuredRoutes (AsServerT App)
securedHandlers = \case
  Sas.Authenticated user -> SecuredRoutes
    { posts = postHandlers
    , comments = commentHandlers
    }
    where ?requestCtx = RequestContext.make user
  _ -> Sas.throwAll Error.unauthorized

type OrderParam = QueryParam "order" Order
type SortByParam = QueryParam "sortBy"
type CursorParam = QueryParam "after" Cursor
type PageSizeParam = QueryParam "pageSize" Int

data PostRoutes mode = PostRoutes
  { getPosts :: mode
      -- GET /posts
      :- QueryParam "authorId" (Id User)
      :> SortByParam Sort
      :> OrderParam
      :> CursorParam
      :> PageSizeParam
      :> Http.Get '[JSON] (Page PostDto)
  , getPost :: mode
      -- GET /posts/:postId
      :- Capture "postId" (Id Post)
      :> Http.Get '[JSON] PostDto
  , createPost :: mode
      -- POST /posts
      :- ReqBody '[JSON] NewPostDto
      :> Http.Post '[JSON] PostIdDto
  , deletePost :: mode
      -- DELETE /posts/:postId
      :- Capture "postId" (Id Post)
      :> Http.Delete '[JSON] Http.NoContent
  , getPostComments :: mode
      -- GET /posts/:postId/comments
      :- Capture "postId" (Id Post)
      :> "comments"
      :> SortByParam Sort
      :> OrderParam
      :> CursorParam
      :> PageSizeParam
      :> Http.Get '[JSON] (Page CommentDto)
  , createPostComment :: mode
      -- POST /posts/:postId/comments
      :- Capture "postId" (Id Post)
      :> "comments"
      :> ReqBody '[JSON] NewCommentDto
      :> Http.Post '[JSON] CommentIdDto
  }
  deriving Generic

postHandlers :: (?requestCtx :: RequestContext)
  => PostRoutes (AsServerT App)
postHandlers = PostRoutes
  { getPosts = PostController.getPosts
  , getPost = PostController.getPost
  , createPost = PostController.createPost
  , deletePost = PostController.deletePost
  , getPostComments = CommentController.getPostComments
  , createPostComment = CommentController.createPostComment
  }

data CommentRoutes mode = CommentRoutes
  { getComment :: mode
      -- GET /comments/:commentId
      :- Capture "comment" (Id Comment)
      :> Http.Get '[JSON] CommentDto
  , deleteComment :: mode
      -- DELETE /comments/:commentId
      :- Capture "commentId" (Id Comment)
      :> Http.Delete '[JSON] Http.NoContent
  , getCommentReplies :: mode
      -- GET /comments/:commentId/comments
      :- Capture "commentId" (Id Comment)
      :> "comments"
      :> SortByParam Sort
      :> OrderParam
      :> CursorParam
      :> PageSizeParam
      :> Http.Get '[JSON] (Page CommentDto)
  , createCommentReply :: mode
      -- POST /comments/:commentId/comments
      :- Capture "commentId" (Id Comment)
      :> "comments"
      :> ReqBody '[JSON] NewCommentDto
      :> Http.Post '[JSON] CommentIdDto
  }
  deriving Generic

commentHandlers :: (?requestCtx :: RequestContext)
  => CommentRoutes (AsServerT App)
commentHandlers = CommentRoutes
  { getComment = CommentController.getComment
  , deleteComment = CommentController.deleteComment
  , getCommentReplies = CommentController.getCommentReplies
  , createCommentReply = CommentController.createCommentReply
  }
