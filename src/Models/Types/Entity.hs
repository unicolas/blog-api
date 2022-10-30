{-# LANGUAGE InstanceSigs #-}

module Models.Types.Entity (Entity(..)) where

import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import Models.Types.Id (Id(..))

data Entity model = Entity (Id model) model
  deriving (Show, Eq)

instance Postgres.FromRow model => Postgres.FromRow (Entity model) where
  fromRow :: Postgres.RowParser (Entity model)
  fromRow = Entity <$> Postgres.field <*> Postgres.fromRow
