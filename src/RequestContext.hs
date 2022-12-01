{-# LANGUAGE RecordWildCards #-}

module RequestContext (RequestContext(..), makeRequestContext) where

import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id)
import Models.User (User(..))

newtype RequestContext = RequestContext
  { userId :: Id User
  }

makeRequestContext :: Entity User -> RequestContext
makeRequestContext (Entity userId _) = RequestContext {..}
