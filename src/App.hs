{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App (App(..), transform) where

import AppContext (AppContext, cacheContext)
import Auth (Blacklist(..))
import AuthClaims (refreshTtl)
import CacheContext (connectionPool)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM, try)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(runReaderT), asks)
import Control.Monad.Reader.Class (MonadReader)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.String (IsString)
import Database.Redis
  (MonadRedis, Redis, exists, expire, liftRedis, runRedis, set)
import Servant (Handler(..))
import Servant.Server (ServerError)
import ThrowAll (ThrowAll(..))

newtype App a = App (ReaderT AppContext IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppContext
    , MonadThrow
    )

-- | Natural transformation from App to Handler
transform :: (?appCtx :: AppContext) => App a -> Handler a
transform (App rt) = Handler . ExceptT . try $ runReaderT rt ?appCtx

instance ThrowAll (App a) where
  throwAll :: ServerError -> App a
  throwAll = throwM

instance MonadRedis App where
  liftRedis :: Redis a -> App a
  liftRedis action = do
    conn <- asks (connectionPool . cacheContext)
    liftIO (runRedis conn action)

instance Blacklist App where
  addToBlacklist :: ByteString -> App ()
  addToBlacklist (makeKey -> key) = set key "1" *> expire key (round refreshTtl)
    & void
    & liftRedis

  isBlacklisted :: ByteString -> App Bool
  isBlacklisted (makeKey -> key) = liftRedis (fromRight False <$> exists key)

makeKey :: (Monoid a, IsString a) => a -> a
makeKey tkn = mconcat ["token:", tkn, ":blacklist"]
