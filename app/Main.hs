{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App
import AppContext (AppContext(..))
import Auth (authHandler, fromSecret, generateKey)
import qualified Configuration.Dotenv as Dotenv
import qualified Controllers.Api as Api
import Controllers.Types.Error (customFormatters)
import Crypto.JOSE (JWK)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import DatabaseContext (DatabaseContext)
import qualified DatabaseContext
import LoggingContext (LoggingContext)
import qualified LoggingContext
import qualified LoggingContext as LoggerConfig (LoggerConfig(..))
import AuthClaims
  (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
import Network.Wai.Handler.Warp (run)
import Servant (Context(..))
import Servant.Server.Generic (genericServeTWithContext)
import qualified System.Environment as Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
  loadEnv
  dbCtx <- makeDbCtxFromEnv
  loggingCtx <- makeLoggingCtxFromEnv
  key <- makeJwkFromEnv
  port <- lookupEnvOrDefault "APP_PORT" 8000
  let ?appCtx = AppContext dbCtx loggingCtx
  let
    ctx = customFormatters
      :. authHandler @AccessClaims key accessSettings
      :. authHandler @RefreshClaims key refreshSettings
      :. EmptyContext
    server = Api.handlers key
    app = genericServeTWithContext App.transform server ctx
  run port app

loadEnv :: IO ()
loadEnv = Dotenv.loadFile Dotenv.defaultConfig

lookupEnvOrDefault :: Read a => String -> a -> IO a
lookupEnvOrDefault var def = do
  maybeEnv <- Environment.lookupEnv var
  pure $ fromMaybe def (maybeEnv >>= readMaybe)

makeDbCtxFromEnv :: IO DatabaseContext
makeDbCtxFromEnv = do
  conn <- fromString <$> Environment.getEnv "DATABASE_URL"
  poolCacheTtl <- lookupEnvOrDefault "POOL_CACHE_TTL" 60
  poolNumStripes <- lookupEnvOrDefault "POOL_NUM_STRIPES" 2
  poolMaxPerStripe <- lookupEnvOrDefault "POOL_MAX_PER_STRIPE" 10
  DatabaseContext.make conn poolCacheTtl poolNumStripes poolMaxPerStripe

makeJwkFromEnv :: IO JWK
makeJwkFromEnv = do
  secret <- Environment.lookupEnv "JWT_SECRET"
  maybe generateKey (pure . fromSecret . fromString) secret

makeLoggingCtxFromEnv :: IO LoggingContext
makeLoggingCtxFromEnv = do
  level <- lookupEnvOrDefault "LOG_LEVEL" LoggingContext.Info
  filename <- lookupEnvOrDefault "LOG_FILENAME" "app"
  timeFormat <- lookupEnvOrDefault "LOG_TIME_FORMAT" LoggingContext.defaultTimeFormat
  bufferSize <- lookupEnvOrDefault "LOG_BUFFER_SIZE" LoggingContext.defaultBufferSize
  LoggingContext.make LoggingContext.LoggerConfig
    { LoggerConfig.level = level
    , LoggerConfig.file = "logs/" <> filename <> ".log"
    , LoggerConfig.timeFormat = timeFormat
    , LoggerConfig.bufferSize = bufferSize
    }
