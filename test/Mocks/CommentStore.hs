{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.CommentStore (CommentStore(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.UUID.V4 (nextRandom)
import Mocks.StorageMock (StorageMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Comment (Comment(..))
import Models.Post (Post)
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id(..))
import Stores.CommentStore (CommentStore(..))

instance CommentStore StorageMock where
  find :: Id Comment -> StorageMock (Maybe (Entity Comment))
  find commentId = gets (Map.lookup commentId . StorageMock.comments)

  findAll :: StorageMock [Entity Comment]
  findAll = gets (Map.elems . StorageMock.comments)

  save :: Comment -> StorageMock (Maybe (Id Comment))
  save comment = do
    commentId <- liftIO (Id <$> nextRandom)
    comments <- gets
      $ Map.insert commentId (Entity commentId comment) . StorageMock.comments
    modify (\s -> s {StorageMock.comments = comments})
    pure $ Just commentId

  delete :: Id Comment -> StorageMock ()
  delete _ = undefined

  findByPost :: Id Post -> StorageMock [Entity Comment]
  findByPost _ = undefined
