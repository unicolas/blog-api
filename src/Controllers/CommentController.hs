{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.CommentController
  ( Routes
  , handlers
  , getComments
  , getComment
  , createComment
  , deleteComment
  ) where

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow(throwM))
import Controllers.Types.Error (Error(..))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import Dto.CommentDto (CommentDto)
import qualified Dto.CommentDto as CommentDto
import Dto.NewCommentDto (NewCommentDto)
import qualified Dto.NewCommentDto as NewCommentDto
import Models.Comment (Comment)
import Models.Post (Post)
import Models.Types.Id (Id)
import qualified Servant as Http (Delete, Get, NoContent(..), Post)
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
import Stores.CommentStore (CommentStore)

type Routes = GetComments :<|> GetComment :<|> CreateComment :<|> DeleteComment

handlers :: (MonadThrow m, CommentStore m) => ServerT Routes m
handlers = getComments :<|> getComment :<|> createComment :<|> deleteComment

type Base = "comments"

-- GET /comments
type GetComments = Base
  :> QueryParam "postId" (Id Post)
  :> Http.Get '[JSON] [CommentDto]

getComments :: (CommentStore m) => Maybe (Id Post) -> m [CommentDto]
getComments maybeId = do
  comments <- maybe CommentStore.findAll CommentStore.findByPost maybeId
  pure $ CommentDto.fromEntity <$> comments

-- GET /comments/:commentId
type GetComment = Base
  :> Capture "comment" (Id Comment)
  :> Http.Get '[JSON] CommentDto

getComment :: (MonadThrow m, CommentStore m) => Id Comment -> m CommentDto
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
  :> ReqBody '[JSON] NewCommentDto
  :> Http.Post '[JSON] (Id Comment)

createComment :: (MonadThrow m, CommentStore m) => NewCommentDto -> m (Id Comment)
createComment comment = do
  maybeId <- CommentStore.save (NewCommentDto.toComment comment)
  case maybeId of
    Just commentId -> pure commentId
    Nothing -> throwM err500
      { errBody = Aeson.encode $ Error "Failed to create comment."
      , errHeaders = [("Content-Type", "application/json")]
      }

-- DELETE /comments/:postId
type DeleteComment = Base
  :> Capture "commentId" (Id Comment)
  :> Http.Delete '[JSON] Http.NoContent

deleteComment :: (MonadThrow m, CommentStore m) => Id Comment -> m Http.NoContent
deleteComment commentId = do
  exists <- isJust <$> CommentStore.find commentId
  unless exists $ throwM err404
    { errBody = Aeson.encode $ Error "Could not find post with such ID."
    , errHeaders = [("Content-Type", "application/json")]
    }
  CommentStore.delete commentId
  pure Http.NoContent
