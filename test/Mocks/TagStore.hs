{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.TagStore () where

import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock
import Models.Post (Post)
import Models.Tag (Tag)
import Models.Types.Id (Id)
import Stores.TagStore (TagStore(..))

instance TagStore AppMock where
  findByPost :: Id Post -> AppMock [Tag]
  findByPost postId = gets (Map.findWithDefault [] postId . AppMock.tags)
