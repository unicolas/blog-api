{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Models.Types.Cursor
 (Cursor(..), decode, encode, cursorExpression, make, fromList) where

import Data.Maybe (listToMaybe)
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding.Base64 (decodeBase64, encodeBase64)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.PostgreSQL.Simple (Query)
import GHC.Records (HasField(getField))
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.Types.Sorting (Order(..), Sort(..), Sorting)
import qualified Servant
import Text.Read (readMaybe)

data Cursor where
  CreatedAtCursor :: (UTCTime, Id model) -> Order -> Cursor
  UpdatedAtCursor :: (UTCTime, Id model) -> Order -> Cursor
  TitleCursor :: (Text, Id model) -> Order -> Cursor

deriving instance Read Cursor
deriving instance Show Cursor

decode :: Text -> Either Text Cursor
decode text = decodeBase64 text
  >>= maybe (Left "Failed to read cursor") Right . readMaybe . unpack

encode :: Cursor -> Text
encode = encodeBase64 . pack . show

instance Servant.FromHttpApiData Cursor where
  parseQueryParam :: Text -> Either Text Cursor
  parseQueryParam = decode

make ::
  ( HasField "createdAt" model UTCTime
  , HasField "updatedAt" model UTCTime
  , HasField "title" model Text
  ) => Sorting -> Entity model -> Cursor
make (sort, order) (Entity id' model) = case sort of
  CreatedAt -> CreatedAtCursor (getField @"createdAt" model, id') order
  UpdatedAt -> UpdatedAtCursor (getField @"updatedAt" model, id') order
  Title -> TitleCursor (getField @"title" model, id') order

cursorExpression :: Cursor -> Query
cursorExpression = \case
  CreatedAtCursor (v, Id i) o -> compose "created_at" o (iso8601Show v) (show i)
  UpdatedAtCursor (v, Id i) o -> compose "updated_at" o (iso8601Show v) (show i)
  TitleCursor (v, Id i) o -> compose "title" o (unpack v) (show i)
  where
    quote str = mconcat ["'", str , "'"]
    compose field order value id' = fromString $ mconcat
      [ "(", field, ", id)"
      , case order of
          Asc -> " >= "
          Desc -> " <= "
      , "(", quote value, ", ", quote id', ")"
      ]

fromList ::
  ( HasField "createdAt" model UTCTime
  , HasField "title" model Text
  , HasField "updatedAt" model UTCTime
  ) => Sorting -> [Entity model] -> Maybe Cursor
fromList sorting entities = make sorting <$> (listToMaybe . reverse) entities
