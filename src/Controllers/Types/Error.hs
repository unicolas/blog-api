{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Types.Error (Error (..), notFound, serverError, unauthorized) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (ServerError(..), err401, err404, err500)

newtype Error = Error { error :: Text } deriving (Generic, ToJSON)

withMessage :: ServerError -> Text -> ServerError
withMessage err message = err {errBody = Aeson.encode (Error message)}

jsonContent :: ServerError -> ServerError
jsonContent err = err {errHeaders = [("Content-Type", "application/json")]}

notFound :: Text -> ServerError
notFound = jsonContent . withMessage err404

serverError :: Text -> ServerError
serverError = jsonContent . withMessage err500

unauthorized :: ServerError
unauthorized = jsonContent err401
