{-# LANGUAGE RecordWildCards #-}

module RequestContext (RequestContext(..), make) where

import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import Models.User (User(..))

newtype RequestContext = RequestContext
  { userId :: Id User
  }

make :: Entity User -> RequestContext
make (Entity userId _) = RequestContext {..}
