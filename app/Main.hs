{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Controllers.Api (api, server)
import Data.ByteString.UTF8 (fromString)
import Network.Wai.Handler.Warp (run)
import Servant (Context(..), serveWithContext)
import Servant.Auth.Server
  (defaultCookieSettings, defaultJWTSettings, fromSecret, generateKey)

main :: IO ()
main = do
  env <- loadFile defaultConfig
  let secret = lookup "JWT_SECRET" env
  key <- case secret of
    Nothing -> generateKey
    Just str -> pure $ fromSecret $ fromString str
  let
    jwtSettings = defaultJWTSettings key
    ctx = defaultCookieSettings :. jwtSettings :. EmptyContext
    app = serveWithContext api ctx (server defaultCookieSettings jwtSettings)
  run 8000 app
