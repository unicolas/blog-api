{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.PostController (Routes, handlers) where

import Control.Monad.IO.Class (MonadIO(liftIO))
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
  , Handler
  , JSON
  , QueryParam
  , ReqBody
  , ServerError(..)
  , err404
  , err500
  , throwError
  , type (:<|>)(..)
  , type (:>)
  )
import Servant.Server (Server)
import qualified Stores.PostStore as PostStore
import Stores.Types.Database (Database(withDatabase))

type Routes = GetPosts :<|> GetPost :<|> CreatePost

handlers :: Server Routes
handlers = getPosts :<|> getPost :<|> createPost

type Base = "posts"

-- GET /posts
type GetPosts = Base
  :> QueryParam "authorId" (Id User)
  :> Http.Get '[JSON] [PostDto]

getPosts :: Maybe (Id User) -> Handler [PostDto]
getPosts maybeId = liftIO $ do
  posts <- withDatabase
    $ maybe PostStore.findAll PostStore.findByAuthor maybeId
  pure $ PostDto.fromEntity <$> posts

-- GET /posts/:postId
type GetPost = Base
  :> Capture "postId" (Id Post)
  :> Http.Get '[JSON] PostDto

getPost :: Id Post -> Handler PostDto
getPost postId = do
  maybePost <- liftIO $ withDatabase $ PostStore.find postId
  let maybeDto = PostDto.fromEntity <$> maybePost
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwError err404
      { errBody = Aeson.encode $ Error "Could not find post with such ID."
      , errHeaders = [("Content-Type", "application/json")]
      }

-- POST /posts
type CreatePost = Base
  :> ReqBody '[JSON] PostDto
  :> Http.Post '[JSON] (Id Post)

createPost :: PostDto -> Handler (Id Post)
createPost post = do
  maybeId <- liftIO $ withDatabase $ PostStore.save $ PostDto.toPost post
  case maybeId of
    Just postId -> pure postId
    Nothing -> throwError err500
      { errBody = Aeson.encode $ Error "Failed to create post."
      , errHeaders = [("Content-Type", "application/json")]
      }
