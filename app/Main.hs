{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App
import AppContext (AppContext(..))
import qualified Configuration.Dotenv as Dotenv
import Controllers.Api (api, server)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import DatabaseContext (makeDatabaseContext)
import Network.Wai.Handler.Warp (run)
import Servant (Context(..), hoistServerWithContext, serveWithContext)
import qualified Servant.Auth.Server as Sas
import qualified System.Environment as Environment
import Text.Read (readMaybe)

main :: IO ()
main = loadEnv *> do
  conn <- Environment.getEnv "DATABASE_URL"
  dbCtx <- makeDatabaseContext (fromString conn)
  let ?appCtx = AppContext dbCtx
  secret <- Environment.lookupEnv "JWT_SECRET"
  key <- maybe Sas.generateKey (pure . Sas.fromSecret . fromString) secret
  maybePort <- Environment.lookupEnv "APP_PORT"
  let port = fromMaybe 8000 (maybePort >>= readMaybe)
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
