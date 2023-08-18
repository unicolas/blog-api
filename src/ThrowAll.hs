{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- From servant-auth-server
module ThrowAll (ThrowAll(..)) where

import Servant (GenericServant, ServerError, ToServant, fromServant, (:<|>)(..))
import Servant.Server.Generic (AsServerT)

class ThrowAll a where
  throwAll :: ServerError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll :: ServerError -> a :<|> b
  throwAll e = throwAll e :<|> throwAll e

instance
  (ThrowAll (ToServant api (AsServerT m)), GenericServant api (AsServerT m))
  => ThrowAll (api (AsServerT m)) where

  throwAll :: ServerError -> api (AsServerT m)
  throwAll = fromServant . throwAll

instance ThrowAll b => ThrowAll (a -> b) where
  throwAll :: ServerError -> a -> b
  throwAll e = const (throwAll e)
