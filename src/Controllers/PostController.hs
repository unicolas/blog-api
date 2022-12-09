{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.PostController
  ( Routes
  , handlers
  , getPosts
  , getPost
  , createPost
  , deletePost
  ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Data.Time (getCurrentTime)
import Dto.NewPostDto (NewPostDto)
import qualified Dto.NewPostDto as NewPostDto
import Dto.PostDto (PostDto)
import qualified Dto.PostDto as PostDto
import Models.Post (Post(..))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import Models.User (User)
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (Delete, Get, NoContent(..), Post)
import Servant
  (Capture, JSON, QueryParam, ReqBody, ServerT, type (:<|>)(..), type (:>))
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)

type Routes = "posts"
  :> (GetPosts :<|> GetPost :<|> CreatePost :<|> DeletePost)

handlers :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m, MonadIO m)
  => ServerT Routes m
handlers = getPosts :<|> getPost :<|> createPost :<|> deletePost

-- GET /posts
type GetPosts =
  QueryParam "authorId" (Id User)
  :> Http.Get '[JSON] [PostDto]

getPosts :: (PostStore m) => Maybe (Id User) -> m [PostDto]
getPosts maybeId = do
  posts <- maybe PostStore.findAll PostStore.findByAuthor maybeId
  pure $ PostDto.fromEntity <$> posts

-- GET /posts/:postId
type GetPost =
  Capture "postId" (Id Post)
  :> Http.Get '[JSON] PostDto

getPost :: (MonadThrow m, PostStore m) => Id Post -> m PostDto
getPost postId = do
  maybePost <- PostStore.find postId
  let maybeDto = PostDto.fromEntity <$> maybePost
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM (Error.notFound "Could not find post with such ID.")

-- POST /posts
type CreatePost =
  ReqBody '[JSON] NewPostDto
  :> Http.Post '[JSON] (Id Post)

createPost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m, MonadIO m)
  => NewPostDto -> m (Id Post)
createPost dto = do
  now <- liftIO getCurrentTime
  let post = NewPostDto.toPost dto (RequestContext.userId ?requestCtx) now now
  PostStore.save post >>= \case
    Just postId -> pure postId
    Nothing -> throwM (Error.serverError "Failed to create post.")

-- DELETE /posts/:postId
type DeletePost =
  Capture "postId" (Id Post)
  :> Http.Delete '[JSON] Http.NoContent

deletePost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m)
  => Id Post -> m Http.NoContent
deletePost postId = PostStore.find postId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Post{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    PostStore.delete postId
    pure Http.NoContent
