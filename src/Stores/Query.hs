{-# LANGUAGE OverloadedStrings #-}

module Stores.Query (fetch, run, runMany) where

import Data.Int (Int64)
import Data.List (intersperse)
import Database.PostgreSQL.Simple
  (Connection, FromRow, Query, ToRow, execute, executeMany, query)

-- | `query` for a split `Query` and `Connection` as last parameter.
fetch :: (ToRow q, FromRow r) => [Query] -> q -> Connection -> IO [r]
fetch sql q conn = query conn (intercalate sql) q

-- | `execute` for a split `Query` and `Connection` as last parameter.
run :: ToRow q => [Query] -> q -> Connection -> IO Int64
run sql q conn = execute conn (intercalate sql) q

-- | `executeMany` for a split `Query` and `Connection` as last parameter.
runMany :: ToRow q => [Query] -> [q] -> Connection -> IO Int64
runMany sql qs conn = executeMany conn (intercalate sql) qs

-- | Intercalates Query with spaces
intercalate :: [Query] -> Query
intercalate = mconcat . intersperse " "
