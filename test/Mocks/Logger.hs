{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.Logger (Logger(..)) where

import Mocks.AppMock (AppMock)
import Control.Monad (void)
import Logger (Logger(..))
import Data.Text (Text)

instance Logger AppMock where
  debug :: Text -> AppMock ()
  debug = void . pure

  info :: Text -> AppMock ()
  info = void . pure

  warn :: Text -> AppMock ()
  warn = void . pure

  error :: Text -> AppMock ()
  error = void . pure
