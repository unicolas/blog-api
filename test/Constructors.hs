{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Constructors (makeId, makeUtc, makeUuid, makeUser) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.UUID (UUID, fromString)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User(..))

makeUtc :: String -> UTCTime
makeUtc = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"

makeUuid :: String -> UUID
makeUuid = fromJust . fromString

makeId :: String -> Id phantom
makeId = Id . makeUuid

makeUser :: Text -> Text -> String -> Entity User
makeUser username email userId = Entity (makeId userId) User {..}
