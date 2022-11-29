{-# LANGUAGE DataKinds #-}
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

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow(throwM))
import qualified Controllers.Types.Error as Error
import Data.Maybe (isJust)
import Dto.NewPostDto (NewPostDto)
import qualified Dto.NewPostDto as NewPostDto
import Dto.PostDto (PostDto)
import qualified Dto.PostDto as PostDto
import Models.Post (Post)
import Models.Types.Id (Id)
import Models.User (User)
import qualified Servant as Http (Delete, Get, NoContent(..), Post)
import Servant
  (Capture, JSON, QueryParam, ReqBody, ServerT, type (:<|>)(..), type (:>))
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)

type Routes = GetPosts :<|> GetPost :<|> CreatePost :<|> DeletePost

handlers :: (MonadThrow m, PostStore m) => ServerT Routes m
handlers = getPosts :<|> getPost :<|> createPost :<|> deletePost

type Base = "posts"

-- GET /posts
type GetPosts = Base
  :> QueryParam "authorId" (Id User)
  :> Http.Get '[JSON] [PostDto]

getPosts :: (PostStore m) => Maybe (Id User) -> m [PostDto]
getPosts maybeId = do
  posts <- maybe PostStore.findAll PostStore.findByAuthor maybeId
  pure $ PostDto.fromEntity <$> posts

-- GET /posts/:postId
type GetPost = Base
  :> Capture "postId" (Id Post)
  :> Http.Get '[JSON] PostDto

getPost :: (MonadThrow m, PostStore m) => Id Post -> m PostDto
getPost postId = do
  maybePost <- PostStore.find postId
  let maybeDto = PostDto.fromEntity <$> maybePost
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM (Error.notFound "Could not find post with such ID.")

-- POST /posts
type CreatePost = Base
  :> ReqBody '[JSON] NewPostDto
  :> Http.Post '[JSON] (Id Post)

createPost :: (MonadThrow m, PostStore m) => NewPostDto -> m (Id Post)
createPost post = do
  maybeId <- PostStore.save $ NewPostDto.toPost post
  case maybeId of
    Just postId -> pure postId
    Nothing -> throwM (Error.serverError "Failed to create post.")

-- DELETE /posts/:postId
type DeletePost = Base
  :> Capture "postId" (Id Post)
  :> Http.Delete '[JSON] Http.NoContent

deletePost :: (MonadThrow m, PostStore m) => Id Post -> m Http.NoContent
deletePost postId = do
  exists <- isJust <$> PostStore.find postId
  unless exists $ throwM (Error.notFound "Could not find post with such ID.")
  PostStore.delete postId
  pure Http.NoContent
