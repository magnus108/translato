module Lib.Server.Serve where

import           Servant.Auth.Server
import           Servant.Server.Generic

import           Control.Monad.Except           ( MonadError )
import           Lib.Server.Handler.GetPermissions
import           Lib.Server.Handler.GetPhotographers
import           Lib.Server.Handler.GetTabs
import           Lib.Server.Handler.GetDocs
import           Lib.Server.Handler.PostLogin
import           Lib.Server.Handler.PostTabs
import           Lib.Server.Handler.PostPhotographers
import           Lib.Api
import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Data.Permission
import          qualified Lib.Server.Error               as Err


siteServer :: Site AppServer
siteServer = Site { publicSite    = genericServerT publicServer
                  , protectedSite = genericServerT protectedServer
                  }

publicServer :: PublicSite AppServer
publicServer =
    PublicSite { postLogin = servePostLogin, getDocs = serveGetDocs }


protectedServer :: ProtectedSite AppServer
protectedServer = ProtectedSite
    { photographers  = genericServerT photographerServer
    , tabSite = genericServerT tabServer
    , getPermissions = withAuthResultAndPermission Simple
                                                   serveGetPermissions
    }

tabServer :: TabSite AppServer
tabServer = TabSite
    { getTabs  = withAuthResultAndPermission Simple serveGetTabs
    , postTabs = withAuthResultAndPermission Simple servePostTabs
    }

photographerServer :: PhotographerSite AppServer
photographerServer = PhotographerSite
    { getPhotographers = withAuthResultAndPermission Simple
                                                   serveGetPhotographers
    , postPhotographers = withAuthResultAndPermission Simple servePostPhotographers
    }




class WithError a where
    throwError :: Err.ServerAppErrorType -> a

instance WithError b => WithError (a -> b) where
    throwError e = const $ throwError e

instance WithError (ServerApp a) where
    throwError e = Err.throwError e


withAuthResult
    :: WithError a => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar = case ar of
    Authenticated ac -> func ac
    _                -> throwError $ Err.ServerError "servantErr"

withAuthResultAndPermission
    :: WithError a
    => Permission
    -> (AuthCookie -> a)
    -> (AuthResult AuthCookie -> a)
withAuthResultAndPermission p func =
    withAuthResult (\ac -> withPermission (permissions ac) p (func ac))

withPermission :: WithError a => [Permission] -> Permission -> a -> a
withPermission ps p func = 
    if elem p ps then func else throwError $ Err.ServerError "servantErr"

