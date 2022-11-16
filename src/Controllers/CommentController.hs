{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.CommentController (Routes, handlers) where

import App (App)
import Control.Monad.Catch (MonadThrow(throwM))
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
  , JSON
  , ReqBody
  , ServerError(..)
  , ServerT
  , err404
  , err500
  , type (:<|>)(..)
  , type (:>)
  )
import Servant.API (QueryParam)
import qualified Stores.CommentStore as CommentStore

type Routes = GetComments :<|> GetComment :<|> CreateComment

handlers :: ServerT Routes App
handlers = getComments :<|> getComment :<|> createComment

type Base = "comments"

-- GET /comments
type GetComments = Base
  :> QueryParam "postId" (Id Post)
  :> Http.Get '[JSON] [CommentDto]

getComments :: Maybe (Id Post) -> App [CommentDto]
getComments maybeId = do
  comments <- maybe CommentStore.findAll CommentStore.findByPost maybeId
  pure $ CommentDto.fromEntity <$> comments

-- GET /comments/:commentId
type GetComment = Base
  :> Capture "comment" (Id Comment)
  :> Http.Get '[JSON] CommentDto

-- getComment :: (MonadThrow m, CommentStore m) => Id Comment -> m CommentDto
getComment :: Id Comment -> App CommentDto
getComment commentId = do
  maybeComment <- CommentStore.find commentId
  let maybeDto = CommentDto.fromEntity <$> maybeComment
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM err404
      { errBody = Aeson.encode $ Error "Could not find comment with such ID."
      , errHeaders = [("Content-Type", "application/json")]
      }

-- POST /comments
type CreateComment = Base
  :> ReqBody '[JSON] CommentDto
  :> Http.Post '[JSON] (Id Comment)

createComment :: CommentDto -> App (Id Comment)
createComment comment = do
  maybeId <- CommentStore.save (CommentDto.toComment comment)
  case maybeId of
    Just commentId -> pure commentId
    Nothing -> throwM err500
      { errBody = Aeson.encode $ Error "Failed to create comment."
      , errHeaders = [("Content-Type", "application/json")]
      }
