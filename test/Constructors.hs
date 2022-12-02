{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Constructors (makeId, makeUtc, makeUuid, serverError) where

import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.UUID (UUID, fromString)
import Models.Types.Id (Id(..))
import Servant (ServerError(..))

makeUtc :: String -> UTCTime
makeUtc = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"

makeUuid :: String -> UUID
makeUuid = fromJust . fromString

makeId :: String -> Id phantom
makeId = Id . makeUuid

serverError :: Int -> ServerError -> Bool
serverError code ServerError{errHTTPCode} = code == errHTTPCode
