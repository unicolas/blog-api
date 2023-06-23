{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Dto.CountDto (CountDto(..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

newtype CountDto = CountDto {count :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON
