{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stores.TagStore (TagStore(..), addTags) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.Functor (void)
import Data.Pool (withResource)
import DatabaseContext (DatabaseContext(..))
import Models.Post (Post)
import Models.Tag (Tag(..))
import Models.Types.Aggregate (Aggregate, aggregate)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import qualified Stores.Query as Query

class Monad m => TagStore m where
  findByPost :: Id Post -> m [Tag]
  save :: [Tag] -> m ()

instance TagStore App where
  findByPost :: Id Post -> App [Tag]
  findByPost idPost = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch ["SELECT post_id, term FROM tags WHERE post_id = ?"] [idPost]
      & withResource pool
      & liftIO

  save :: [Tag] -> App ()
  save tags = do
    pool <- asks (connectionPool . databaseContext)
    Query.runMany ["INSERT INTO tags (post_id, term) VALUES (?,?)"] (rowMap tags)
      & withResource pool
      & liftIO
      & void
    where
      rowMap = fmap (\Tag {..} -> (postId, term))

addTags :: TagStore m => Entity Post -> m (Aggregate Post [Tag])
addTags post@(Entity postId _) = aggregate (pure post, findByPost postId)
