{-# LANGUAGE RecordWildCards #-}

module AppContext (AppContext(..), make) where

import CacheContext (CacheContext)
import DatabaseContext (DatabaseContext)
import LoggingContext (LoggingContext)

data AppContext = AppContext
  { databaseContext :: !DatabaseContext
  , loggingContext :: !LoggingContext
  , cacheContext :: !CacheContext
  }

make :: DatabaseContext -> LoggingContext -> CacheContext -> AppContext
make databaseContext loggingContext cacheContext = AppContext {..}
