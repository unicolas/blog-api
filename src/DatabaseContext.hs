{-# LANGUAGE RecordWildCards #-}

module DatabaseContext (DatabaseContext(..), make) where

import Data.ByteString.UTF8 (ByteString)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple as Postgres

data DatabaseContext = DatabaseContext
  { connectionPool :: Pool Postgres.Connection
  , connectionString :: !ByteString
  }

make :: ByteString -> NominalDiffTime -> Int -> Int -> IO DatabaseContext
make connectionString poolCacheTtl poolNumStripes poolMaxPerStripe = do
  connectionPool <- Pool.createPool
    (connectPostgreSQL connectionString)
    close
    poolNumStripes
    poolCacheTtl
    poolMaxPerStripe
  pure DatabaseContext {..}
