{-# OPTIONS_GHC -Wno-orphans #-}

module Mocks.TokenStore (TokenStore(..)) where

import Control.Monad.State (gets, modify)
import Data.ByteString.UTF8 (ByteString)
import Data.Time (UTCTime)
import Mocks.AppMock (AppMock)
import qualified Mocks.AppMock as AppMock
import Stores.TokenStore (TokenStore(..))

instance TokenStore AppMock where
  blacklist :: ByteString -> UTCTime -> AppMock ()
  blacklist token _ = do
    newBlacklist <- gets ((token:) . AppMock.blacklist)
    modify (\s -> s {AppMock.blacklist = newBlacklist})

  isBlacklisted :: ByteString -> AppMock Bool
  isBlacklisted token = gets (elem token . AppMock.blacklist)
