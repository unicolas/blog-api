module Logger (Logger(..), logDebug, logInfo, logWarn, logError) where

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
  logMsg :: LogLevel -> Text -> m ()

instance Logger App where
  logMsg :: LogLevel -> Text -> App ()
  logMsg lvl msg = do
    LoggingContext {level, logger} <- asks loggingContext
    when (level <= lvl) (logger compose)
      & liftIO
    where
      compose time = mconcat
        [ toLogStr "["
        , toLogStr time
        , toLogStr ("] [" <> map toUpper (show lvl) <> "] ")
        , toLogStr msg
        , toLogStr "\n"
        ]

logDebug :: Logger m => Text -> m ()
logDebug = logMsg Debug

logInfo :: Logger m => Text -> m ()
logInfo = logMsg Info

logWarn :: Logger m => Text -> m ()
logWarn = logMsg Warn

logError :: Logger m => Text -> m ()
logError = logMsg Error
