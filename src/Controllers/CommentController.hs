{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Controllers.Types.Error as Error
import Dto.CommentDto (CommentDto)
import qualified Dto.CommentDto as CommentDto
import Dto.NewCommentDto (NewCommentDto)
import qualified Dto.NewCommentDto as NewCommentDto
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (Delete, Get, NoContent(..), Post)
import Servant (Capture, JSON, ReqBody, ServerT, type (:<|>)(..), type (:>))
import Servant.API (QueryParam)
import qualified Stores.CommentStore as CommentStore
import Stores.CommentStore (CommentStore)

type Routes = GetComments :<|> GetComment :<|> CreateComment :<|> DeleteComment

handlers :: (?requestCtx :: RequestContext, MonadThrow m, CommentStore m)
  => ServerT Routes m
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
    Nothing -> throwM (Error.notFound "Could not find comment with such ID.")

-- POST /comments
type CreateComment = Base
  :> ReqBody '[JSON] NewCommentDto
  :> Http.Post '[JSON] (Id Comment)

createComment :: (?requestCtx :: RequestContext, MonadThrow m, CommentStore m)
  => NewCommentDto -> m (Id Comment)
createComment dto = do
  let comment = NewCommentDto.toComment dto (RequestContext.userId ?requestCtx)
  CommentStore.save comment >>= \case
    Just commentId -> pure commentId
    Nothing -> throwM (Error.serverError "Failed to create comment.")

-- DELETE /comments/:postId
type DeleteComment = Base
  :> Capture "commentId" (Id Comment)
  :> Http.Delete '[JSON] Http.NoContent

deleteComment :: (?requestCtx :: RequestContext, MonadThrow m, CommentStore m)
  => Id Comment -> m Http.NoContent
deleteComment commentId = CommentStore.find commentId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Comment{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    CommentStore.delete commentId
    pure Http.NoContent
