{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Controllers.Api (handlers) where

import App (App)
import Controllers.AuthController (LoginRequest, LoginResponse)
import qualified Controllers.AuthController as AuthController
import qualified Controllers.CommentController as CommentController
import qualified Controllers.PostController as PostController
import qualified Controllers.Types.Error as Error
import qualified Controllers.UserController as UserController
import Crypto.JWT (JWK)
import Dto.CommentDto (CommentDto, CommentIdDto, NewCommentDto)
import Dto.CountDto (CountDto)
import Dto.Page (Page)
import Dto.PostDto (NewPostDto, PostDto, PostIdDto)
import Dto.UserDto (NewUserDto, UserDto, UserIdDto)
import GHC.Generics (Generic)
import AuthClaims (AccessClaims, RefreshClaims, subjectClaim)
import Models.Comment (Comment)
import Models.Post (Post)
import Models.Types.Cursor (Cursor)
import Models.Types.Id (Id)
import Models.Types.Sorting (Order, Sort)
import Models.User (User(..))
import RequestContext (RequestContext(..))
import qualified Servant as Http (Delete, Get, NoContent(..), Post)
import Servant
  (AuthProtect, Capture, JSON, NamedRoutes, QueryParam, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Server.Generic (AsServerT)
import ThrowAll (throwAll)

type Json = '[JSON]

type AuthJwtAccess = AuthProtect "jwt-access"
type AuthJwtRefresh = AuthProtect "jwt-refresh"

type instance AuthServerData AuthJwtAccess = AccessClaims
type instance AuthServerData AuthJwtRefresh = RefreshClaims

data Api mode = Api
  { login :: mode
      -- POST /login
      :- "login"
      :> ReqBody Json LoginRequest
      :> Http.Post Json LoginResponse
  , signup :: mode
      -- POST /signup
      :- "signup"
      :> ReqBody Json NewUserDto
      :> Http.Post Json UserIdDto
  , secured :: mode
      :- AuthJwtAccess
      :> NamedRoutes SecuredRoutes
  }
  deriving Generic

handlers :: JWK -> Api (AsServerT App)
handlers jwk = Api
  { login = AuthController.login jwk
  , signup = UserController.createUser
  , secured = securedHandlers
  }

data SecuredRoutes mode = SecuredRoutes
  { posts :: mode
      :- "posts"
      :> NamedRoutes PostRoutes
  , comments :: mode
      :- "comments"
      :> NamedRoutes CommentRoutes
  , users :: mode
      :- "users"
      :> NamedRoutes UserRoutes
  }
  deriving (Generic)

securedHandlers :: AccessClaims -> SecuredRoutes (AsServerT App)
securedHandlers (subjectClaim -> Just userId) = SecuredRoutes
    { posts = postHandlers
    , comments = commentHandlers
    , users = userHandlers
    }
    where ?requestCtx = RequestContext userId
securedHandlers _ = throwAll Error.unauthorized

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
      :> Http.Get Json (Page PostDto)
  , getPost :: mode
      -- GET /posts/:postId
      :- Capture "postId" (Id Post)
      :> Http.Get Json PostDto
  , createPost :: mode
      -- POST /posts
      :- ReqBody Json NewPostDto
      :> Http.Post Json PostIdDto
  , deletePost :: mode
      -- DELETE /posts/:postId
      :- Capture "postId" (Id Post)
      :> Http.Delete Json Http.NoContent
  , getPostComments :: mode
      -- GET /posts/:postId/comments
      :- Capture "postId" (Id Post)
      :> "comments"
      :> SortByParam Sort
      :> OrderParam
      :> CursorParam
      :> PageSizeParam
      :> Http.Get Json (Page CommentDto)
  , createPostComment :: mode
      -- POST /posts/:postId/comments
      :- Capture "postId" (Id Post)
      :> "comments"
      :> ReqBody Json NewCommentDto
      :> Http.Post Json CommentIdDto
    , countComments :: mode
      -- GET /posts/:postId/comments/count
      :- Capture "postId" (Id Post)
      :> "comments"
      :> "count"
      :> Http.Get Json CountDto
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
  , countComments = CommentController.countComments
  }

data CommentRoutes mode = CommentRoutes
  { getComment :: mode
      -- GET /comments/:commentId
      :- Capture "comment" (Id Comment)
      :> Http.Get Json CommentDto
  , deleteComment :: mode
      -- DELETE /comments/:commentId
      :- Capture "commentId" (Id Comment)
      :> Http.Delete Json Http.NoContent
  , getCommentReplies :: mode
      -- GET /comments/:commentId/comments
      :- Capture "commentId" (Id Comment)
      :> "comments"
      :> SortByParam Sort
      :> OrderParam
      :> CursorParam
      :> PageSizeParam
      :> Http.Get Json (Page CommentDto)
  , createCommentReply :: mode
      -- POST /comments/:commentId/comments
      :- Capture "commentId" (Id Comment)
      :> "comments"
      :> ReqBody Json NewCommentDto
      :> Http.Post Json CommentIdDto
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

data UserRoutes mode = UserRoutes
  { getUser :: mode
      -- GET /users/:userId
      :- Capture "userId" (Id User)
      :> Http.Get Json UserDto
  , getCurrent :: mode
      -- GET /users/current
      :- "current"
      :> Http.Get Json UserDto
  }
  deriving Generic

userHandlers :: (?requestCtx :: RequestContext) => UserRoutes (AsServerT App)
userHandlers = UserRoutes
  { getUser = UserController.getUser
  , getCurrent = UserController.getCurrentUser
  }
