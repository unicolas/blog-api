{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Stores.Types.Entity (Entity(..)) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import GHC.Generics (Generic)
import Stores.Types.Id (Id(..))

data Entity model = Entity (Id model) model
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Postgres.FromRow model => Postgres.FromRow (Entity model) where
  fromRow :: Postgres.RowParser (Entity model)
  fromRow = Entity <$> Postgres.field <*> Postgres.fromRow
