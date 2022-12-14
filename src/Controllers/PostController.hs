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
import Data.Time (getCurrentTime)
import Dto.NewPostDto (NewPostDto)
import qualified Dto.NewPostDto as NewPostDto
import Dto.PostDto (PostDto)
import qualified Dto.PostDto as PostDto
import Models.Post (Post(..))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import Models.Types.Sorting (Order, Sort)
import qualified Models.Types.Sorting as Sorting
import Models.User (User)
import qualified RequestContext
import RequestContext (RequestContext)
import qualified Servant as Http (NoContent(..))
import qualified Stores.PostStore as PostStore
import Stores.PostStore (PostStore)

getPosts :: (PostStore m)
  => Maybe (Id User) -> Maybe Sort -> Maybe Order -> m [PostDto]
getPosts maybeId maybeSort maybeOrder = do
  let sorting = Sorting.make maybeSort maybeOrder
  posts <- maybe PostStore.findAll PostStore.findByAuthor maybeId sorting
  pure (PostDto.fromEntity <$> posts)

getPost :: (MonadThrow m, PostStore m) => Id Post -> m PostDto
getPost postId = do
  maybePost <- PostStore.find postId
  let maybeDto = PostDto.fromEntity <$> maybePost
  case maybeDto of
    Just dto -> pure dto
    Nothing -> throwM (Error.notFound "Could not find post with such ID.")

createPost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m, MonadIO m)
  => NewPostDto -> m (Id Post)
createPost dto = do
  now <- liftIO getCurrentTime
  let post = NewPostDto.toPost dto (RequestContext.userId ?requestCtx) now now
  PostStore.save post >>= \case
    Just postId -> pure postId
    Nothing -> throwM (Error.serverError "Failed to create post.")

deletePost :: (?requestCtx :: RequestContext, MonadThrow m, PostStore m)
  => Id Post -> m Http.NoContent
deletePost postId = PostStore.find postId >>= \case
  Nothing -> throwM (Error.notFound "Could not find post with such ID.")
  Just (Entity _ Post{userId}) -> do
    when (userId /= RequestContext.userId ?requestCtx) $ throwM Error.forbidden
    PostStore.delete postId
    pure Http.NoContent
