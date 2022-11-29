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
import Controllers.Types.Error (Error(..))
import qualified Data.Aeson as Aeson
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
  ( Capture
  , JSON
  , QueryParam
  , ReqBody
  , ServerError(..)
  , ServerT
  , err404
  , err500
  , type (:<|>)(..)
  , type (:>)
  )
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
    Nothing -> throwM err404
      { errBody = Aeson.encode $ Error "Could not find post with such ID."
      , errHeaders = [("Content-Type", "application/json")]
      }

-- POST /posts
type CreatePost = Base
  :> ReqBody '[JSON] NewPostDto
  :> Http.Post '[JSON] (Id Post)

createPost :: (MonadThrow m, PostStore m) => NewPostDto -> m (Id Post)
createPost post = do
  maybeId <- PostStore.save $ NewPostDto.toPost post
  case maybeId of
    Just postId -> pure postId
    Nothing -> throwM err500
      { errBody = Aeson.encode $ Error "Failed to create post."
      , errHeaders = [("Content-Type", "application/json")]
      }

-- DELETE /posts/:postId
type DeletePost = Base
  :> Capture "postId" (Id Post)
  :> Http.Delete '[JSON] Http.NoContent

deletePost :: (MonadThrow m, PostStore m) => Id Post -> m Http.NoContent
deletePost postId = do
  exists <- isJust <$> PostStore.find postId
  unless exists $ throwM err404
    { errBody = Aeson.encode $ Error "Could not find post with such ID."
    , errHeaders = [("Content-Type", "application/json")]
    }
  PostStore.delete postId
  pure Http.NoContent
