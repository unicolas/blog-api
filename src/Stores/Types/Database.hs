{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Stores.Types.Database (Database(..), connection) where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)

newtype Database a = Database { withDatabase :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

connection :: IO Connection
connection = connectPostgreSQL connectionString
  where
    connectionString = "postgresql://postgres:postgres@host.docker.internal:5432/blog-api"
