{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.PostController (Routes, handlers) where

import App (App)
import Control.Monad.Catch (MonadThrow(throwM))
import Controllers.Types.Error (Error(..))
import qualified Data.Aeson as Aeson
import Dto.PostDto (PostDto)
import qualified Dto.PostDto as PostDto
import Models.Post (Post)
import Models.Types.Id (Id)
import Models.User (User)
import qualified Servant as Http (Get, Post)
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

type Routes = GetPosts :<|> GetPost :<|> CreatePost

handlers :: ServerT Routes App
handlers = getPosts :<|> getPost :<|> createPost

type Base = "posts"

-- GET /posts
type GetPosts = Base
  :> QueryParam "authorId" (Id User)
  :> Http.Get '[JSON] [PostDto]

getPosts :: Maybe (Id User) -> App [PostDto]
getPosts maybeId = do
  posts <- maybe PostStore.findAll PostStore.findByAuthor maybeId
  pure $ PostDto.fromEntity <$> posts

-- GET /posts/:postId
type GetPost = Base
  :> Capture "postId" (Id Post)
  :> Http.Get '[JSON] PostDto

getPost :: Id Post -> App PostDto
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
  :> ReqBody '[JSON] PostDto
  :> Http.Post '[JSON] (Id Post)

createPost :: PostDto -> App (Id Post)
createPost post = do
  maybeId <- PostStore.save $ PostDto.toPost post
  case maybeId of
    Just postId -> pure postId
    Nothing -> throwM err500
      { errBody = Aeson.encode $ Error "Failed to create post."
      , errHeaders = [("Content-Type", "application/json")]
      }
