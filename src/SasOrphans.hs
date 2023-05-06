{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- SAS instances needed to work with NamedRoutes
-- https://github.com/haskell-servant/servant/pull/1531

module SasOrphans () where

import GHC.Generics (Generic, Rep)
import Servant (NamedRoutes, ServerError, ServerT)
import Servant.API.Generic
  ( GServantProduct
  , GenericServant
  , ToServant
  , ToServantApi
  , fromServant
  , toServant
  )
import Servant.Auth.Server (ThrowAll(throwAll))
import Servant.Auth.Server.Internal.AddSetCookie
  (AddSetCookieApi, AddSetCookies(addSetCookies), Nat(S), SetCookieList)
import Servant.Server.Generic (AsServerT)

instance
  ( ThrowAll (ToServant api (AsServerT m))
  , GenericServant api (AsServerT m)
  )
  => ThrowAll (api (AsServerT m)) where

  throwAll :: ServerError -> api (AsServerT m)
  throwAll = fromServant . throwAll

type instance
  AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)

instance {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (ServerT (ToServantApi api) m) cookiedApi
  , Generic (api (AsServerT m))
  , GServantProduct (Rep (api (AsServerT m)))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  )
  => AddSetCookies ('S n) (api (AsServerT m)) cookiedApi where

  addSetCookies :: SetCookieList ('S n) -> api (AsServerT m) -> cookiedApi
  addSetCookies cookies = addSetCookies cookies . toServant
