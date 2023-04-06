{-# LANGUAGE RecordWildCards #-}

module AppContext (AppContext(..), make) where

import DatabaseContext (DatabaseContext)
import LoggingContext (LoggingContext)

data AppContext = AppContext
  { databaseContext :: !DatabaseContext
  , loggingContext :: !LoggingContext
  }

make :: DatabaseContext -> LoggingContext -> AppContext
make databaseContext loggingContext = AppContext {..}
