{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.PostController
  ( getPosts
  , getPost
  , createPost
  , deletePost
  ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Controllers.Types.Error as Error
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (getCurrentTime)
import Dto.Page (Page(..), defaultPageSize)
import qualified Dto.Page as Page
import Dto.PostDto (NewPostDto, PostDto, PostIdDto(..))
import qualified Dto.PostDto as PostDto
import Models.Post (Post(..))
import Models.Types.Cursor (Cursor(..))
import qualified Models.Types.Cursor as Cursor
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Order, Sort(..))
import qualified Models.Types.Sorting as Sorting
import Models.User (User)
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (NoContent(..))
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)

getPosts :: PostStore m
  => Maybe (Id User)
  -> Maybe Sort
  -> Maybe Order
  -> Maybe Cursor
  -> Maybe Int
  -> m (Page PostDto)
getPosts maybeId maybeSort maybeOrder maybeCursor maybePageSize = do
  let
    sorting = Sorting.make maybeSort maybeOrder
    pageSize = fromMaybe defaultPageSize maybePageSize
  posts <- maybe PostStore.findAll PostStore.findByAuthor maybeId
    sorting maybeCursor (1 + pageSize)
  let nextCursor = Cursor.make sorting <$> (listToMaybe . reverse) posts
  pure $ Page.make (PostDto.fromEntity <$> posts) nextCursor pageSize

getPost :: (MonadThrow m, PostStore m) => Id Post -> m PostDto
getPost postId = do
  maybePost <- PostStore.find postId
  let maybeDto = PostDto.fromEntity <$> maybePost
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM (Error.notFound "Could not find post with such ID.")

createPost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m, MonadIO m)
  => NewPostDto -> m PostIdDto
createPost dto = do
  now <- liftIO getCurrentTime
  let post = PostDto.toPost dto (RequestContext.userId ?requestCtx) now now
  PostStore.save post >>= \case
    Just (Id postId) -> pure PostIdDto {postId}
    Nothing -> throwM (Error.serverError "Failed to create post.")

deletePost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m)
  => Id Post -> m Http.NoContent
deletePost postId = PostStore.find postId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Post{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    PostStore.delete postId
    pure Http.NoContent
