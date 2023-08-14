{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Function ((&))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Dto.Page (Page(..))
import qualified Dto.Page as Page
import Dto.PostDto (NewPostDto(..), PostDto, PostIdDto(..))
import qualified Dto.PostDto as PostDto
import Models.Post (Post(..))
import Models.Tag (Tag(..))
import Models.Types.Aggregate (Aggregate(..))
import Models.Types.Cursor (Cursor(..))
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
import Models.Types.Sorting (Order, Sort(..))
import Models.User (User)
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (NoContent(..))
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)
import qualified Stores.TagStore as TagStore
import Stores.TagStore (TagStore)

getPosts :: (PostStore m, TagStore m)
  => Maybe (Id User)
  -> Maybe Sort
  -> Maybe Order
  -> Maybe Cursor
  -> Maybe Int
  -> m (Page PostDto)
getPosts maybeId maybeSort maybeOrder maybeCursor maybePageSize = do
  posts <- maybe PostStore.findAllWithTags PostStore.findByAuthorWithTags maybeId
    pagination
  pure $ Page.make (dtos posts) (nextCursor posts) (count - 1)
  where
    pagination@Pagination {..} = defaultPagination
      & withSort maybeSort
      & withOrder maybeOrder
      & withCursor maybeCursor
      & withCount (fmap (+1) maybePageSize)
    dtos = fmap PostDto.fromAggregate
    nextCursor = Cursor.fromList (sort, order) . fmap (\(Aggregate e _) -> e)

getPost :: (MonadThrow m, PostStore m, TagStore m) => Id Post -> m PostDto
getPost postId = PostStore.findWithTags postId >>= \case
  Just post -> pure (PostDto.fromAggregate post)
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")

createPost :: (?requestCtx :: RequestContext)
  => (MonadThrow m, PostStore m, TagStore m, MonadIO m)
  => NewPostDto -> m PostIdDto
createPost dto = do
  now <- liftIO getCurrentTime
  let
    post = PostDto.toPost dto (RequestContext.userId ?requestCtx) now now
    terms = fromMaybe [] (tags dto)
  PostStore.save post >>= \case
    Just postId -> createTags postId terms $> PostIdDto postId
    Nothing -> throwM (Error.serverError "Failed to create post.")

deletePost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m)
  => Id Post -> m Http.NoContent
deletePost postId = PostStore.find postId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Post{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    PostStore.delete postId
    pure Http.NoContent

createTags :: (TagStore m) => Id Post -> [Text] -> m ()
createTags postId = TagStore.save . fmap (\term -> Tag {..})
