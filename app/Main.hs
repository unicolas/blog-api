{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App
import AppContext (AppContext(..))
import qualified Configuration.Dotenv as Dotenv
import qualified Controllers.Api as Api
import Controllers.Types.Error (customFormatters)
import Crypto.JOSE (JWK)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import DatabaseContext (DatabaseContext)
import qualified DatabaseContext
import Network.Wai.Handler.Warp (run)
import Servant (Context(..))
import qualified Servant.Auth.Server as Sas
import Servant.Server.Generic (genericServeTWithContext)
import qualified System.Environment as Environment
import Text.Read (readMaybe)

main :: IO ()
main = loadEnv *> do
  dbCtx <- makeDbCtxFromEnv
  key <- makeJwkFromEnv
  port <- lookupEnvOrDefault "APP_PORT" 8000
  let ?appCtx = AppContext dbCtx
  let
    jwtSettings = Sas.defaultJWTSettings key
    cookieSettings = Sas.defaultCookieSettings
    ctx = cookieSettings :. jwtSettings :. customFormatters :. EmptyContext
    server = Api.handlers cookieSettings jwtSettings
    app = genericServeTWithContext App.transform server ctx
  run port app

loadEnv :: IO [(String, String)]
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
  maybe Sas.generateKey (pure . Sas.fromSecret . fromString) secret
