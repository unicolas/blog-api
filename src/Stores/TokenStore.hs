{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Stores.TokenStore (TokenStore (..)) where

import App (App)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.UTF8 (ByteString)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.String (IsString)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.Redis (exists, expire, liftRedis, set)

class Monad m => TokenStore m where
  blacklist :: ByteString -> UTCTime -> m ()
  isBlacklisted :: ByteString -> m Bool

instance TokenStore App where
  blacklist :: ByteString -> UTCTime -> App ()
  blacklist (makeKey -> key) time = do
    seconds <- liftIO (round . diffUTCTime time <$> getCurrentTime)
    liftRedis (set key "1" *> expire key seconds)
      & void

  isBlacklisted :: ByteString -> App Bool
  isBlacklisted (makeKey -> key) = fromRight False <$> liftRedis (exists key)

makeKey :: (Monoid a, IsString a) => a -> a
makeKey tkn = mconcat ["token:", tkn, ":blacklist"]
