{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Auth (authHandler, signToken, generateKey, fromSecret) where

import qualified App
import AppContext (AppContext)
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.JOSE
  ( JWK
  , KeyMaterialGenParam(OctGenParam)
  , bestJWSAlg
  , decodeCompact
  , fromOctets
  , genJWK
  , newJWSHeader
  , runJOSE
  )
import Crypto.JWT
  (HasClaimsSet, JWTError, JWTValidationSettings, SignedJWT, signJWT, verifyJWT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as ByteString
import Network.Wai (Request, requestHeaders)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Stores.TokenStore (TokenStore(isBlacklisted))
import Utility (maybeRight, (<<*))

authHandler :: (?appCtx :: AppContext)
  => (HasClaimsSet a, FromJSON a)
  => JWK -> JWTValidationSettings -> AuthHandler Request (Maybe (a, ByteString))
authHandler jwk settings = mkAuthHandler $ \case
  (getToken -> Just token) -> App.transform (verifyToken jwk settings token)
  _ -> pure Nothing

getToken :: Request -> Maybe ByteString
getToken req = do
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  pure token
  where
    split = ByteString.break (== ' ') >>> second (ByteString.drop 1)

verifyToken :: (HasClaimsSet a, FromJSON a, TokenStore m, MonadIO m)
  => JWK -> JWTValidationSettings -> ByteString -> m (Maybe (a, ByteString))
verifyToken jwk settings token = runVerification <<* checkBlacklist
  where
    runVerification = liftIO $ do
      validation <- runJOSE @JWTError (decode token >>= verifyJWT settings jwk)
      pure ((,token) <$> maybeRight validation)
    checkBlacklist = guard . not <$> isBlacklisted token
    decode = decodeCompact . LazyByteString.fromString . ByteString.toString

signToken :: (ToJSON a) => JWK -> a -> IO (Maybe SignedJWT)
signToken jwk claims = maybeRight <$> runJOSE @JWTError sign
  where
    sign = do
      alg <- bestJWSAlg jwk
      signJWT jwk (newJWSHeader ((), alg)) claims

generateKey :: IO JWK
generateKey = genJWK (OctGenParam 256)

fromSecret :: ByteString -> JWK
fromSecret = fromOctets
