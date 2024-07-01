{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.Blacklist (Blacklist(..)) where

import Auth (Blacklist(..))
import Control.Monad.State (gets, modify)
import Data.ByteString (ByteString)
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock

instance Blacklist AppMock where
  addToBlacklist :: ByteString -> AppMock ()
  addToBlacklist token = do
    newBlacklist <- gets ((token:) . AppMock.blacklist)
    modify (\s -> s {AppMock.blacklist = newBlacklist})

  isBlacklisted :: ByteString -> AppMock Bool
  isBlacklisted token = gets (elem token . AppMock.blacklist)
