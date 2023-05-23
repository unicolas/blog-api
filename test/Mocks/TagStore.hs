{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.TagStore (TagStore(..)) where

import Control.Monad.State (gets, modify)
import Data.List (union)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock
import Models.Post (Post)
import Models.Tag (Tag(..))
import Models.Types.Id (Id)
import Stores.TagStore (TagStore(..))

instance TagStore AppMock where
  findByPost :: Id Post -> AppMock [Tag]
  findByPost postId = gets (Map.findWithDefault [] postId . AppMock.tags)

  save :: [Tag] -> AppMock ()
  save tags = case postId <$> listToMaybe tags of
    Just id' -> do
      newTags <- gets (Map.insertWith union id' tags . AppMock.tags)
      modify (\s -> s {AppMock.tags = newTags})
    Nothing -> pure ()
