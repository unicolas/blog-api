{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Auth (authHandler, signToken, generateKey, Blacklist(..), pass, revoke, fromSecret) where

import Control.Applicative (liftA2)
import Control.Arrow (second, (>>>))
import Control.Monad (guard, unless)
import Control.Monad.IO.Class (liftIO)
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Network.Wai (Request, requestHeaders)
import Servant (Handler(..))
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Utility (maybeRight)

authHandler :: (HasClaimsSet a, FromJSON a)
  => JWK
  -> JWTValidationSettings
  -> (ByteString -> m Bool)
  -> (forall b. m b -> Handler b)
  -> AuthHandler Request (Maybe a)
authHandler jwk settings accept nt = mkAuthHandler $ \case
  (getToken -> Just token) -> liftA2 (<*)
    (verifyToken jwk settings token)
    (runAccept accept nt token)
  _ -> pure Nothing

getToken :: Request -> Maybe ByteString
getToken req = do
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  pure token
  where
    split = Char8.break (== ' ') >>> second (Char8.drop 1)

runAccept :: (ByteString -> m Bool)
  -> (forall a. m a -> Handler a)
  -> ByteString
  -> Handler (Maybe ())
runAccept accept nt = fmap guard . nt . accept

verifyToken :: (HasClaimsSet a, FromJSON a)
  => JWK
  -> JWTValidationSettings
  -> ByteString
  -> Handler (Maybe a)
verifyToken jwk settings token = liftIO (maybeRight <$> runJOSE @JWTError verify)
  where
    verify = decode token >>= verifyJWT settings jwk
    decode = ByteString.fromStrict >>> decodeCompact

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

class Monad m => Blacklist m where
  isBlacklisted :: ByteString -> m Bool
  addToBlacklist :: ByteString -> m ()

-- | Accepts and revokes a token if not revoked already, rejects it otherwise.
revoke :: Blacklist m => ByteString -> m Bool
revoke token = do
  blacklisted <- isBlacklisted token
  unless blacklisted (addToBlacklist token)
  pure (not blacklisted)

-- | Always accepts a token.
pass :: Applicative f => ByteString -> f Bool
pass _ = pure True
