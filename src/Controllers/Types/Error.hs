{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.Types.Error (Error (..)) where

import Data.Aeson.Types (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Error = Error { error :: Text } deriving (Generic, ToJSON)
