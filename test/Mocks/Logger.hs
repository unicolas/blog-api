{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.Logger (Logger(..)) where

import Control.Monad (void)
import Data.Text (Text)
import Logger (Logger(..))
import Mocks.AppMock (AppMock)

instance Logger AppMock where
  debug :: Text -> AppMock ()
  debug = void . pure

  info :: Text -> AppMock ()
  info = void . pure

  warn :: Text -> AppMock ()
  warn = void . pure

  error :: Text -> AppMock ()
  error = void . pure
