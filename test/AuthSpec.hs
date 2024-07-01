{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import Auth (authHandler, generateKey, signToken)
import Control.Lens ((&), (?~))
import Crypto.JOSE (JWK, encodeCompact)
import Crypto.JWT
  ( Audience(Audience)
  , ClaimsSet
  , claimAud
  , defaultJWTValidationSettings
  , emptyClaimsSet
  )
import Data.ByteString (ByteString, toStrict)
import Data.Maybe (fromJust, isJust, isNothing)
import Network.Wai (Request(requestHeaders), defaultRequest)
import Servant (runHandler)
import Servant.Server.Experimental.Auth (AuthHandler(unAuthHandler))
import Test.Hspec (Spec, before, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  before generateKeyAndToken $ do
    describe "Given a valid JWT" $ do
      it "Authorises the request if the token is accepted" $ \(jwk, jwt) -> do
        let
          pass _ = pure True
          authHeader = ("Authorization" , "Bearer " <> jwt)
          request = defaultRequest {requestHeaders = [authHeader]}
          settings = defaultJWTValidationSettings (== "test")
          handler = unAuthHandler (authHandler @ClaimsSet jwk settings pass id)

        (Right result) <- runHandler (handler request)

        result `shouldSatisfy` isJust

      it "Rejects the request if the token is not accepted" $ \(jwk, jwt) -> do
        let
          block _ = pure False
          authHeader = ("Authorization" , "Bearer " <> jwt)
          request = defaultRequest {requestHeaders = [authHeader]}
          settings = defaultJWTValidationSettings (== "test")
          handler = unAuthHandler (authHandler @ClaimsSet jwk settings block id)

        (Right result) <- runHandler (handler request)

        result `shouldSatisfy` isNothing

  before generateKeyAndToken $ do
    describe "Given an invalid JWT" $ do
      it "Rejects the request" $ \(jwk, jwt) -> do
        let
          pass _ = pure True
          authHeader = ("Authorization" , "Bearer " <> jwt)
          request = defaultRequest {requestHeaders = [authHeader]}
          settings = defaultJWTValidationSettings (/= "test")
          handler = unAuthHandler (authHandler @ClaimsSet jwk settings pass id)

        (Right result) <- runHandler (handler request)

        result `shouldSatisfy` isNothing

generateKeyAndToken :: IO (JWK, ByteString)
generateKeyAndToken = do
  let claims = emptyClaimsSet & claimAud ?~ Audience ["test"]
  jwk <- generateKey
  jwt <- toStrict . encodeCompact . fromJust <$> signToken jwk claims
  pure (jwk, jwt)
