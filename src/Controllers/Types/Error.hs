{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Types.Error (Error (..), notFound, serverError) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (ServerError(..), err404)
import Servant.Server (err500)

newtype Error = Error { error :: Text } deriving (Generic, ToJSON)

notFound :: Text -> ServerError
notFound message = err404
  { errBody = Aeson.encode (Error message)
  , errHeaders = [("Content-Type", "application/json")]
  }

serverError :: Text -> ServerError
serverError message = err500
  { errBody = Aeson.encode (Error message)
  , errHeaders = [("Content-Type", "application/json")]
  }
