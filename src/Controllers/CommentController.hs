{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Controllers.CommentController
  ( getComment
  , createPostComment
  , createCommentReply
  , deleteComment
  , getPostComments
  , getCommentReplies
  ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime)
import Dto.CommentDto (CommentDto, CommentIdDto(..), NewCommentDto(..))
import qualified Dto.CommentDto as CommentDto
import Dto.Page (Page)
import qualified Dto.Page as Page
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Cursor (Cursor)
import qualified Models.Types.Cursor as Cursor
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.Types.Pagination
  ( Pagination(..)
  , defaultPagination
  , withCount
  , withCursor
  , withOrder
  , withSort
  )
import Models.Types.Sorting (Order, Sort)
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (NoContent(..))
import qualified Stores.CommentStore as CommentStore
import Stores.CommentStore (CommentStore)
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)

getComment :: (MonadThrow m, CommentStore m) => Id Comment -> m CommentDto
getComment commentId = do
  maybeComment <- CommentStore.find commentId
  let maybeDto = CommentDto.fromEntity <$> maybeComment
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM (Error.notFound "Could not find comment with such ID.")

createPostComment :: (?requestCtx :: RequestContext)
  => (MonadThrow m, CommentStore m, PostStore m, MonadIO m)
  => Id Post -> NewCommentDto -> m CommentIdDto
createPostComment postId dto = do
  maybePost <- PostStore.find postId
  when (isNothing maybePost)
    $ throwM (Error.notFound "Could not find post with such ID.")
  createComment postId Nothing dto

createCommentReply :: (?requestCtx :: RequestContext)
  => (MonadThrow m, CommentStore m, MonadIO m)
  => Id Comment -> NewCommentDto -> m CommentIdDto
createCommentReply parentId dto = CommentStore.find parentId >>= \case
  Nothing -> throwM (Error.notFound "Could not find comment with such ID.")
  Just (Entity _ Comment {postId}) -> createComment postId (Just parentId) dto

deleteComment :: (?requestCtx :: RequestContext, MonadThrow m, CommentStore m)
  => Id Comment -> m Http.NoContent
deleteComment commentId = CommentStore.find commentId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Comment{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    CommentStore.delete commentId
    pure Http.NoContent

getPostComments :: (CommentStore m)
  => Id Post
  -> Maybe Sort
  -> Maybe Order
  -> Maybe Cursor
  -> Maybe Int
  -> m (Page CommentDto)
getPostComments = getCommentsBy CommentStore.findByPost

getCommentReplies :: (CommentStore m)
  => Id Comment
  -> Maybe Sort
  -> Maybe Order
  -> Maybe Cursor
  -> Maybe Int
  -> m (Page CommentDto)
getCommentReplies = getCommentsBy CommentStore.findByComment

getCommentsBy :: CommentStore m
  => (Id model -> Pagination -> m [Entity Comment])
  -> Id model
  -> Maybe Sort
  -> Maybe Order
  -> Maybe Cursor
  -> Maybe Int
  -> m (Page CommentDto)
getCommentsBy find modelId maybeSort maybeOrder maybeCursor maybePageSize = do
  comments <- find modelId pagination
  pure $ Page.make
    (CommentDto.fromEntity <$> comments)
    (Cursor.fromList (sort, order) comments)
    (count - 1)
  where
    pagination@Pagination {..} = defaultPagination
      & withSort maybeSort
      & withOrder maybeOrder
      & withCursor maybeCursor
      & withCount (fmap (+1) maybePageSize)

createComment :: (?requestCtx::RequestContext)
  => (MonadIO m, CommentStore m, MonadThrow m)
  => Id Post -> Maybe (Id Comment) -> NewCommentDto -> m CommentIdDto
createComment postId maybeParent dto = do
  now <- liftIO getCurrentTime
  let
    userId = RequestContext.userId ?requestCtx
    comment = CommentDto.toComment dto userId postId maybeParent now now
  CommentStore.save comment >>= \case
    Just (Id commentId) -> pure CommentIdDto {commentId}
    Nothing -> throwM (Error.serverError "Failed to create comment.")
