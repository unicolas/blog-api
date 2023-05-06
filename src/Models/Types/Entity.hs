{-# LANGUAGE DeriveAnyClass #-}

module Models.Types.Entity (Entity(..)) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import GHC.Generics (Generic)
import Models.Types.Id (Id(..))
import qualified Servant.Auth.Server as Sas

data Entity model = Entity (Id model) model
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Sas.ToJWT, Sas.FromJWT)

instance Postgres.FromRow model => Postgres.FromRow (Entity model) where
  fromRow :: Postgres.RowParser (Entity model)
  fromRow = Entity <$> Postgres.field <*> Postgres.fromRow
