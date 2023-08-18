module RequestContext (RequestContext(..)) where

import Models.Types.Id (Id)
import Models.User (User)

newtype RequestContext = RequestContext {userId :: Id User}
