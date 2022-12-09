{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App
import AppContext (AppContext(..))
import qualified Configuration.Dotenv as Dotenv
import Controllers.Api (api, server)
import Crypto.JOSE (JWK)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import DatabaseContext (DatabaseContext)
import qualified DatabaseContext
import Network.Wai.Handler.Warp (run)
import Servant (Context(..), hoistServerWithContext, serveWithContext)
import qualified Servant.Auth.Server as Sas
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
    ctx = cookieSettings :. jwtSettings :. EmptyContext
    ctxProxy = Proxy :: Proxy '[Sas.CookieSettings, Sas.JWTSettings]
    serverWithCtx = server cookieSettings jwtSettings
    hoistServer = hoistServerWithContext api ctxProxy App.transform serverWithCtx
    app = serveWithContext api ctx hoistServer
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
