{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LoggingContext
  ( LoggerConfig(..)
  , LoggingContext(..)
  , LogLevel(..)
  , defaultBufferSize
  , defaultTimeFormat
  , make
  ) where

import Control.Monad (join, void)
import System.Log.FastLogger
  ( BufSize
  , LogType'(LogFileTimedRotate)
  , TimeFormat
  , TimedFastLogger
  , TimedFileLogSpec(..)
  , defaultBufSize
  , newTimeCache
  , newTimedFastLogger
  )

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Read, Eq, Ord)

data LoggerConfig = LoggerConfig
  { level :: !LogLevel
  , file :: !FilePath
  , timeFormat :: !TimeFormat
  , bufferSize :: !BufSize
  }

data LoggingContext = LoggingContext
  { level :: !LogLevel
  , logger :: !TimedFastLogger
  , cleanUp :: IO ()
  }

make :: LoggerConfig -> IO LoggingContext
make LoggerConfig {..} = do
  (logger, cleanUp) <- newTimedFastLogger formattedTime rotatedLogFile
  pure LoggingContext {..}
  where
    formattedTime = join (newTimeCache timeFormat)
    rotatedLogFile = LogFileTimedRotate
      TimedFileLogSpec
        { timed_log_file = file
        , timed_timefmt = "%F"
        , timed_same_timeframe = (==)
        , timed_post_process = void . pure
        }
      bufferSize

defaultBufferSize :: BufSize
defaultBufferSize = defaultBufSize

defaultTimeFormat :: TimeFormat
defaultTimeFormat = "%FT%T%z"
