{-# LANGUAGE RecordWildCards #-}

module AppContext (AppContext(..), makeAppContext) where

import DatabaseContext (DatabaseContext)

newtype AppContext = AppContext
  { databaseContext :: DatabaseContext
  }

makeAppContext :: DatabaseContext -> AppContext
makeAppContext databaseContext = AppContext {..}
