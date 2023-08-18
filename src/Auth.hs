{-# LANGUAGE OverloadedStrings #-}

module Auth (authHandler, signToken, generateKey, fromSecret) where

import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Controllers.Types.Error as Error
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

authHandler :: (HasClaimsSet a, FromJSON a)
  => JWK -> JWTValidationSettings -> AuthHandler Request a
authHandler jwk settings = mkAuthHandler handleAuth
  where
    handleAuth req = checkAuth (getToken req)
      >>= liftIO . verifyToken jwk settings
      >>= checkAuth
    checkAuth = maybe (throwError Error.unauthorized) pure

getToken :: Request -> Maybe ByteString
getToken req = do
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  pure token
  where
    split = ByteString.break (== ' ') >>> second (ByteString.drop 1)

verifyToken :: (HasClaimsSet a, FromJSON a)
  => JWK -> JWTValidationSettings -> ByteString -> IO (Maybe a)
verifyToken jwk settings token = fromRight <$> runJOSE @JWTError verify
  where
    verify = decodeCompact lazy >>= verifyJWT settings jwk
    lazy = LazyByteString.fromString (ByteString.toString token)

signToken :: (ToJSON a) => JWK -> a -> IO (Maybe SignedJWT)
signToken jwk claims = fromRight <$> runJOSE @JWTError sign
  where
    sign = do
      alg <- bestJWSAlg jwk
      signJWT jwk (newJWSHeader ((), alg)) claims

generateKey :: IO JWK
generateKey = genJWK (OctGenParam 256)

fromSecret :: ByteString -> JWK
fromSecret = fromOctets

fromRight :: Either a b -> Maybe b
fromRight = either (const Nothing) Just
