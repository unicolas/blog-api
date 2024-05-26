{-# LANGUAGE RecordWildCards #-}

module CacheContext (CacheContext(..), makeCacheCtx) where

import Data.Either (fromRight)
import Database.Redis
  (Connection, checkedConnect, defaultConnectInfo, parseConnectInfo)

data CacheContext = CacheContext
  { connectionPool :: Connection
  , connectionString :: !String
  }

makeCacheCtx :: String -> IO CacheContext
makeCacheCtx connectionString = do
  connectionPool <- checkedConnect
    $ fromRight defaultConnectInfo (parseConnectInfo connectionString)
  pure CacheContext {..}
