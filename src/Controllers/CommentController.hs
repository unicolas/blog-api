{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.CommentController (Routes, handlers) where

import Control.Monad.IO.Class (liftIO)
import Controllers.Types.Error (Error(..))
import qualified Data.Aeson as Aeson
import Dto.CommentDto (CommentDto)
import qualified Dto.CommentDto as CommentDto
import Models.Comment (Comment)
import Models.Post (Post)
import Models.Types.Id (Id)
import qualified Servant as Http (Get, Post)
import Servant
  ( Capture
  , Handler
  , JSON
  , ReqBody
  , ServerError(..)
  , err404
  , err500
  , throwError
  , type (:<|>)(..)
  , type (:>)
  )
import Servant.API (QueryParam)
import Servant.Server (Server)
import qualified Stores.CommentStore as CommentStore
import Stores.Types.Database (Database(withDatabase))

type Routes = GetComments :<|> GetComment :<|> CreateComment

handlers :: Server Routes
handlers = getComments :<|> getComment :<|> createComment

type Base = "comments"

-- GET /comments
type GetComments = Base
  :> QueryParam "postId" (Id Post)
  :> Http.Get '[JSON] [CommentDto]

getComments :: Maybe (Id Post) -> Handler [CommentDto]
getComments maybeId = liftIO $ do
  comments <- withDatabase
    $ maybe CommentStore.findAll CommentStore.findByPost maybeId
  pure $ CommentDto.fromEntity <$> comments

-- GET /comments/:commentId
type GetComment = Base
  :> Capture "comment" (Id Comment)
  :> Http.Get '[JSON] CommentDto

getComment :: Id Comment -> Handler CommentDto
getComment commentId = do
  maybeComment <- liftIO $ withDatabase $ CommentStore.find commentId
  let maybeDto = CommentDto.fromEntity <$> maybeComment
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwError err404
      { errBody = Aeson.encode $ Error "Could not find comment with such ID."
      , errHeaders = [("Content-Type", "application/json")]
      }

-- POST /comments
type CreateComment = Base
  :> ReqBody '[JSON] CommentDto
  :> Http.Post '[JSON] (Id Comment)

createComment :: CommentDto -> Handler (Id Comment)
createComment comment = do
  maybeId <- liftIO
    $ withDatabase
    $ CommentStore.save (CommentDto.toComment comment)
  case maybeId of
    Just commentId -> pure commentId
    Nothing -> throwError err500
      { errBody = Aeson.encode $ Error "Failed to create comment."
      , errHeaders = [("Content-Type", "application/json")]
      }
