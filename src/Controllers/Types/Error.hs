{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Types.Error
  ( Error(..)
  , notFound
  , serverError
  , unauthorized
  , forbidden
  , customFormatters
  , badRequest
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (ToJSON)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Network.Wai (rawPathInfo)
import qualified Servant as ErrorFormatters (ErrorFormatters(..))
import Servant
  ( ErrorFormatter
  , ErrorFormatters
  , NotFoundErrorFormatter
  , ServerError(..)
  , defaultErrorFormatters
  , err400
  , err401
  , err403
  , err404
  , err500
  )

newtype Error = Error { error :: Text }
  deriving stock Generic
  deriving anyclass ToJSON

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

forbidden :: ServerError
forbidden = jsonContent err403

badRequest :: Text ->  ServerError
badRequest = jsonContent . withMessage err400

notFoundErrorFormatter :: NotFoundErrorFormatter
notFoundErrorFormatter req = notFound (path <> " not found")
  where path = decodeUtf8 (rawPathInfo req)

customErrorFormatter :: ErrorFormatter
customErrorFormatter _ _ = badRequest . pack

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { ErrorFormatters.notFoundErrorFormatter = notFoundErrorFormatter
  , ErrorFormatters.urlParseErrorFormatter = customErrorFormatter
  , ErrorFormatters.bodyParserErrorFormatter = customErrorFormatter
  , ErrorFormatters.headerParseErrorFormatter = customErrorFormatter
  }
