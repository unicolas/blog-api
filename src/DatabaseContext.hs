{-# LANGUAGE RecordWildCards #-}

module DatabaseContext (DatabaseContext(..), make) where

import Data.ByteString.UTF8 (ByteString)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple as Postgres

data DatabaseContext = DatabaseContext
  { connectionPool :: Pool Postgres.Connection
  , connectionString :: !ByteString
  }

make :: ByteString -> IO DatabaseContext
make connectionString = do
  connectionPool <- Pool.createPool (connectPostgreSQL connectionString) close 3 60 10
  pure DatabaseContext {..}
