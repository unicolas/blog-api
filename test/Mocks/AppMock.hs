module Mocks.AppMock
  ( runMock
  , AppMock
  , Storage(..)
  , emptyStorage
  , storeFromList
  ) where

import Control.Monad.State (StateT, runStateT)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Models.Comment (Comment)
import Models.Credentials (Credentials)
import Models.Post (Post)
import Models.Tag (Tag)
import Models.Types.Entity (Entity(Entity))
import Models.Types.Id (Id)
import Models.User (User)

data Storage = MakeStorage
  { users :: !(Map (Id User) (Entity User))
  , credentials :: !(Map (Id User) Credentials)
  , posts :: !(Map (Id Post) (Entity Post))
  , comments :: !(Map (Id Comment) (Entity Comment))
  , tags :: !(Map (Id Post) [Tag])
  , blacklist :: [ByteString]
  }

type AppMock = StateT Storage IO

runMock :: AppMock a -> Storage -> IO a
runMock st kv = fst <$> runStateT st kv

emptyStorage :: Storage
emptyStorage = MakeStorage
  { users = Map.empty
  , credentials = Map.empty
  , posts = Map.empty
  , comments = Map.empty
  , tags = Map.empty
  , blacklist = []
  }

storeFromList :: [(Id model, model)] -> Map (Id model) (Entity model)
storeFromList = Map.fromList . fmap (\(i, m) -> (i, Entity i m))
