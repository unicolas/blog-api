module Mocks.StorageMock (runMock, StorageMock, Storage(..), emptyStorage) where

import Control.Monad.State (StateT, runStateT)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Models.Comment (Comment)
import Models.Credentials (Credentials)
import Models.Post (Post)
import Models.Types.Entity (Entity)
import Models.Types.Id (Id)
import Models.User (User)

data Storage = MakeStorage
  { users :: !(Map (Id User) (Entity User))
  , credentials :: !(Map (Id User) Credentials)
  , posts :: !(Map (Id Post) (Entity Post))
  , comments :: !(Map (Id Comment) (Entity Comment))
  }

type StorageMock = StateT Storage IO

runMock :: StorageMock a -> Storage -> IO a
runMock st kv = fst <$> runStateT st kv

emptyStorage :: Storage
emptyStorage = MakeStorage
  { users = Map.empty
  , credentials = Map.empty
  , posts = Map.empty
  , comments = Map.empty
  }
