{-# LANGUAGE InstanceSigs #-}

module Logger (Logger (..)) where

import App (App)
import AppContext (AppContext(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Char (toUpper)
import Data.Function ((&))
import Data.Text (Text)
import LoggingContext (LogLevel(..), LoggingContext(..))
import System.Log.FastLogger (ToLogStr(toLogStr))

class Monad m => Logger m where
  debug :: Text -> m ()
  info :: Text -> m ()
  warn :: Text -> m ()
  error :: Text -> m ()

instance Logger App where
  debug :: Text -> App ()
  debug = logMsg Debug

  info :: Text -> App ()
  info = logMsg Info

  warn :: Text -> App ()
  warn = logMsg Warn

  error :: Text -> App ()
  error = logMsg Error

logMsg :: LogLevel -> Text -> App ()
logMsg lvl msg = do
  ctx <- asks loggingContext
  when (level ctx <= lvl) (logger ctx compose *> cleanUp ctx)
    & liftIO
  where
    compose time = mconcat
      [ toLogStr "["
      , toLogStr time
      , toLogStr ("] [" <> map toUpper (show lvl) <> "] ")
      , toLogStr msg
      , toLogStr "\n"
      ]
