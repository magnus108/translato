module Lib.Server.Serve where

import           Servant.Auth.Server
import           Servant.Server.Generic

import           Lib.Server.Handler.GetPermissions
import           Lib.Server.Handler.GetPhotographers
import           Lib.Server.Handler.GetTabs
import           Lib.Server.Handler.GetDocs
import           Lib.Server.Handler.PostLogin
import           Lib.Api
import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Data.Permission
import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )


siteServer :: Site AppServer
siteServer = Site { publicSite    = genericServerT publicServer
                  , protectedSite = genericServerT protectedServer
                  }

publicServer :: PublicSite AppServer
publicServer =
    PublicSite { postLogin = servePostLogin, getDocs = serveGetDocs }


protectedServer :: ProtectedSite AppServer
protectedServer = ProtectedSite
    { photographers  = withAuthResultAndPermission Simple
                                                   serveGetPhotographers
    , tabs  = withAuthResultAndPermission Simple
                                                   serveGetTabs
    , getPermissions = withAuthResultAndPermission Simple
                                                   serveGetPermissions
    }


withAuthResult
    :: WithError m => (AuthCookie -> m a) -> (AuthResult AuthCookie -> m a)
withAuthResult func ar = case ar of
    Authenticated ac -> func ac
    _                -> throwError $ ServerError "servantErr"

withAuthResultAndPermission
    :: WithError m
    => Permission
    -> (AuthCookie -> m a)
    -> (AuthResult AuthCookie -> m a)
withAuthResultAndPermission p func =
    withAuthResult (\ac -> withPermission (permissions ac) p =<< (func ac))

withPermission :: WithError m => [Permission] -> Permission -> a -> m a
withPermission ps p func = do
    traceShowM "fuck"
    if elem p ps then return func else throwError $ ServerError "servantErr"

