{-# LANGUAGE DerivingStrategies #-}

module Models.Types.Id (Id(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField, fromField)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, parseUrlPiece)

newtype Id phantom = Id UUID
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData (Id phantom) where
  parseUrlPiece :: Text -> Either Text (Id phantom)
  parseUrlPiece t = Id <$> parseUrlPiece t

instance ToField (Id phantom) where
  toField :: Id phantom -> Action
  toField (Id value) = toField value

instance FromField (Id phantom) where
  fromField :: FieldParser (Id phantom)
  fromField f s = Id <$> fromField f s
