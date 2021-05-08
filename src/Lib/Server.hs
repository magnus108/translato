module Lib.Server
    ( application
    , docs
    )
where

import           Servant.API.Generic            ( toServant )
import           Servant.Server                 ( Application
                                                , Server
                                                , Handler
                                                , hoistServer
                                                , serve
                                                )

import           Lib.App                        ( AppEnv
                                                , App
                                                , runApp
                                                )

import           Lib.Server.Auth                ( AuthApi
                                                , authServer
                                                , AuthSite
                                                )


import Servant.Auth.Server (JWTSettings, CookieSettings)
import Servant.Auth.Docs
import qualified Servant.Docs                  as Docs

import           Servant.API                   as Web
                                                ( (:>)
                                                , Capture
                                                , Get
                                                , Header
                                                , Header'
                                                , JSON
                                                , NoContent(NoContent)
                                                , Post
                                                , QueryParam
                                                , QueryParam'
                                                , ReqBody
                                                , (:<|>)
                                                )
import           Servant

import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                , ToServant
                                                )

import           Lib.Server.Types               ( AppServer
                                                , ToApi
                                                , Permission
                                                , ProtectAPI
                                                )
import Lib.Server.Protected.AccessKey 




runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    -- not save
    liftIO $ runApp env app

siteServer :: Site AppServer
siteServer = Site { protectedSite      = undefined -- loginHandler
                  }


server :: AppEnv -> Server SiteApi
server env = 
    hoistServerWithContext
        siteAPI
        (Proxy :: Proxy SiteContext)
        (runAppAsHandler env)
        (toServant siteServer)

type SiteContext = '[CookieSettings, JWTSettings]

docs :: Docs.API
docs = Docs.docs siteAPI

application :: AppEnv -> Application
application env = serveWithContext siteAPI (siteContext env) (server env)


siteContext :: AppEnv -> Context SiteContext
siteContext env = undefined --envCookieSettings :. envJWTSettings :. EmptyContext

-------------------------------------------------------------------------------
siteAPI :: Proxy SiteApi
siteAPI = Proxy

type SiteApi = ToApi Site

data Site route = Site
      { protectedSite :: !(route :- ToApi ProtectedSite)
      }
  deriving (Generic)

type ProtectedAPI = ToApi ProtectedSite

data ProtectedSite route
  = ProtectedSite
      { protectedAccessKeySite :: !(route :- "access-key" :> ToApi ProtectedAccessKeySite)
      , getPermissions :: !(route :- GetPermissions)
      }
  deriving (Generic)

type GetPermissions = ProtectAPI :> "permissions" :> Get '[JSON] ([Permission])
