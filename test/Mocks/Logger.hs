{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.Logger (Logger(..)) where

import Data.Text (Text)
import Logger (Logger(..))
import LoggingContext (LogLevel)
import Mocks.AppMock (AppMock)

instance Logger AppMock where
  logMsg :: LogLevel -> Text -> AppMock ()
  logMsg _ _ = pure ()
