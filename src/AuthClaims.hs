{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthClaims
  ( AccessClaims(..)
  , RefreshClaims(..)
  , accessClaims
  , accessSettings
  , refreshClaims
  , refreshSettings
  , subjectClaim
  , refreshTtl
  ) where

import Control.Lens (Lens', view, (?~))
import Crypto.JWT
  ( Audience(..)
  , ClaimsSet
  , HasClaimsSet(..)
  , JWTValidationSettings
  , NumericDate(..)
  , claimExp
  , claimIat
  , claimSub
  , defaultJWTValidationSettings
  , emptyClaimsSet
  , string
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.String (fromString)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Data.UUID as Uuid
import GHC.Generics (Generic)
import Models.Types.Id (Id(..))
import Models.User (User)

newtype AccessClaims = AccessClaims ClaimsSet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasClaimsSet AccessClaims where
  claimsSet :: Lens' AccessClaims ClaimsSet
  claimsSet f (AccessClaims claims) = AccessClaims <$> f claims

accessClaims :: Id User -> UTCTime -> AccessClaims
accessClaims (Id userId) issuedAt = emptyClaimsSet
  & claimSub ?~ fromString (Uuid.toString userId)
  & claimIat ?~ NumericDate issuedAt
  & claimExp ?~ NumericDate (addUTCTime 900 issuedAt)
  & claimAud ?~ Audience ["access"]
  & AccessClaims

accessSettings :: JWTValidationSettings
accessSettings = defaultJWTValidationSettings (== "access")

newtype RefreshClaims = RefreshClaims ClaimsSet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasClaimsSet RefreshClaims where
  claimsSet :: Lens' RefreshClaims ClaimsSet
  claimsSet f (RefreshClaims claims) = RefreshClaims <$> f claims

refreshClaims :: Id User -> UTCTime -> RefreshClaims
refreshClaims (Id userId) issuedAt = emptyClaimsSet
  & claimSub ?~ fromString (Uuid.toString userId)
  & claimIat ?~ NumericDate issuedAt
  & claimExp ?~ NumericDate (addUTCTime refreshTtl issuedAt)
  & claimAud ?~ Audience ["refresh"]
  & RefreshClaims

refreshSettings :: JWTValidationSettings
refreshSettings = defaultJWTValidationSettings (== "refresh")

subjectClaim :: HasClaimsSet a => a -> Maybe (Id User)
subjectClaim c = Id <$> (view claimSub c >>= Uuid.fromText . view string)

refreshTtl :: NominalDiffTime
refreshTtl = 86400
