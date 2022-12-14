{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Controllers.CommentController
  ( getComments
  , getComment
  , createComment
  , deleteComment
  ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime)
import Dto.CommentDto (CommentDto)
import qualified Dto.CommentDto as CommentDto
import Dto.NewCommentDto (NewCommentDto(..))
import qualified Dto.NewCommentDto as NewCommentDto
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Order, Sort)
import qualified Models.Types.Sorting as Sorting
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (NoContent(..))
import qualified Stores.CommentStore as CommentStore
import Stores.CommentStore (CommentStore)
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)

getComments :: (CommentStore m)
  => Maybe (Id Post) -> Maybe Sort -> Maybe Order -> m [CommentDto]
getComments maybeId maybeSort maybeOrder = do
  let sorting = Sorting.make maybeSort maybeOrder
  comments <- maybe CommentStore.findAll CommentStore.findByPost maybeId sorting
  pure $ CommentDto.fromEntity <$> comments

getComment :: (MonadThrow m, CommentStore m) => Id Comment -> m CommentDto
getComment commentId = do
  maybeComment <- CommentStore.find commentId
  let maybeDto = CommentDto.fromEntity <$> maybeComment
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM (Error.notFound "Could not find comment with such ID.")

createComment :: (?requestCtx :: RequestContext)
  => (MonadThrow m, CommentStore m, PostStore m, MonadIO m)
  => NewCommentDto -> m (Id Comment)
createComment dto@NewCommentDto{..} = do
  maybePost <- PostStore.find (Id postId)
  when (isNothing maybePost)
    $ throwM (Error.notFound "Could not find post with such ID.")
  now <- liftIO getCurrentTime
  let
    userId = RequestContext.userId ?requestCtx
    comment = NewCommentDto.toComment dto userId now now
  CommentStore.save comment >>= \case
    Just commentId -> pure commentId
    Nothing -> throwM (Error.serverError "Failed to create comment.")

deleteComment :: (?requestCtx :: RequestContext, MonadThrow m, CommentStore m)
  => Id Comment -> m Http.NoContent
deleteComment commentId = CommentStore.find commentId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Comment{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    CommentStore.delete commentId
    pure Http.NoContent
