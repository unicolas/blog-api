{-# LANGUAGE ImplicitParams #-}

module App (App(..), transform) where

import AppContext (AppContext)
import Control.Monad.Catch (MonadThrow, throwM, try)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Servant (Handler(..))
import Servant.Server (ServerError)
import ThrowAll (ThrowAll(..))

newtype App a = App (ReaderT AppContext IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppContext
    , MonadThrow
    )

-- | Natural transformation from App to Handler
transform :: (?appCtx :: AppContext) => App a -> Handler a
transform (App rt) = Handler . ExceptT . try $ runReaderT rt ?appCtx

instance ThrowAll (App a) where
  throwAll :: ServerError -> App a
  throwAll = throwM
