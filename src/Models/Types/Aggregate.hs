{-# LANGUAGE InstanceSigs #-}

module Models.Types.Aggregate (Aggregate(..)) where

import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import Models.Types.Entity (Entity(..))

data Aggregate a b = Aggregate (Entity a) b
  deriving (Show, Eq)

instance
  ( Postgres.FromRow a
  , Postgres.FromRow b
  ) => Postgres.FromRow (Aggregate a b) where
  fromRow :: Postgres.RowParser (Aggregate a b)
  fromRow = Aggregate <$> Postgres.fromRow <*> Postgres.fromRow
