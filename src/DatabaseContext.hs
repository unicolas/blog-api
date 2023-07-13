{-# LANGUAGE RecordWildCards #-}

module DatabaseContext (DatabaseContext(..), make) where

import Data.ByteString.UTF8 (ByteString)
import Data.Function ((&))
import Data.Pool (Pool)
import Data.Pool as Pool (defaultPoolConfig, newPool, setNumStripes)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple as Postgres

data DatabaseContext = DatabaseContext
  { connectionPool :: Pool Postgres.Connection
  , connectionString :: !ByteString
  }

make :: ByteString -> Double -> Int -> Int -> IO DatabaseContext
make connectionString poolCacheTtl poolNumStripes poolMaxPerStripe = do
  let create = connectPostgreSQL connectionString
  connectionPool <- newPool
    $ defaultPoolConfig create close poolCacheTtl poolMaxPerStripe
    & setNumStripes (Just poolNumStripes)
  pure DatabaseContext {..}
