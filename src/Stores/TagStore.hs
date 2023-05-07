{-# LANGUAGE OverloadedStrings #-}

module Stores.TagStore (TagStore(..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.Pool (withResource)
import DatabaseContext (DatabaseContext(..))
import Models.Post (Post)
import Models.Tag (Tag)
import Models.Types.Id (Id)
import qualified Stores.Query as Query

class Monad m => TagStore m where
  findByPost :: Id Post -> m [Tag]

instance TagStore App where
  findByPost :: Id Post -> App [Tag]
  findByPost idPost = do
    pool <- asks (connectionPool . databaseContext)
    Query.fetch
        ["SELECT post_id, term FROM tags WHERE post_id = ?"]
        [idPost]
      & withResource pool
      & liftIO
