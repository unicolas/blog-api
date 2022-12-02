{-# LANGUAGE RecordWildCards #-}

module AppContext (AppContext(..), make) where

import DatabaseContext (DatabaseContext)

newtype AppContext = AppContext
  { databaseContext :: DatabaseContext
  }

make :: DatabaseContext -> AppContext
make databaseContext = AppContext {..}
