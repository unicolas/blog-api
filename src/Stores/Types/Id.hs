{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Stores.Types.Id (Id(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import GHC.Generics (Generic)
import qualified Servant

newtype Id phantom = Id { unwrap :: UUID }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Servant.FromHttpApiData (Id phantom) where
  parseUrlPiece :: Text -> Either Text (Id phantom)
  parseUrlPiece t = Id <$> Servant.parseUrlPiece t

instance Postgres.ToField (Id phantom) where
  toField :: Id phantom -> Postgres.Action
  toField (Id value) = Postgres.toField value

instance Postgres.FromField (Id phantom) where
  fromField :: Postgres.FieldParser (Id phantom)
  fromField f s = Id <$> Postgres.fromField f s
