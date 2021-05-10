module Lib.Server
    ( application
    , docs
    )
where

import           Lib.App.Error                  ( throwError
                                                , AppErrorType(..)
                                                , WithError(..)
                                                )
import           Servant.API.Generic            ( toServant )
import           Servant.Server                 ( Application
                                                , Server
                                                , Handler
                                                , hoistServer
                                                , serve
                                                )

import           Lib.App                        ( AppEnv
                                                , App
                                                , Env(..)
                                                , runApp
                                                )

import           Lib.Server.Auth                ( AuthApi
                                                , authServer
                                                , AuthSite
                                                )
import           Lib.Server.Types               ( AuthCookie(..)
                                                , Permission(..)
                                                )


import           Servant.Auth.Server            ( JWTSettings
                                                , CookieSettings
                                                )
import           Servant.Auth.Docs
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
import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           Servant.Auth.Server
import           Servant.Server.Generic         ( genericServerT )

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
import           Lib.Server.Protected.AccessKey


import           Network.Wai.Middleware.Cors


runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    -- not save
    liftIO $ runApp env app

-- does not match layercake
siteServer :: Site AppServer
siteServer = Site { protectedSite = genericServerT protectedServer }

-- does not match layercake
protectedServer :: ProtectedSite AppServer
protectedServer = ProtectedSite
    { protectedAccessKeySite = genericServerT protectedAccessKeyServer
    , getPermissions = withAuthResultAndPermission PermitAdd serveGetPermissions
    }

protectedAccessKeyServer :: ProtectedAccessKeySite AppServer
protectedAccessKeyServer = ProtectedAccessKeySite
    { postAddAccessKey = withAuthResultAndPermission PermitAdd servePostAddAccessKey
    , getAccessKey     = withAuthResultAndPermission PermitAdd serveGetAccessKey
    , getAccessKeys = withAuthResultAndPermission PermitAdd serveGetAccessKeys
    , deleteAccessKey = withAuthResultAndPermission PermitAdd serveDeleteAccessKey
    }

serveGetPermissions :: AuthCookie -> App [Permission]
serveGetPermissions AuthCookie {..} = pure permissions

serveDeleteAccessKey :: AuthCookie -> AccessKeyUUID -> App NoContent
serveDeleteAccessKey AuthCookie {..} uuid = do
    undefined
  --runDb $ deleteWhere [AccessKeyIdentifier ==. uuid]
  --pure NoContent

serveGetAccessKey :: AuthCookie -> AccessKeyUUID -> App AccessKeyInfo
serveGetAccessKey AuthCookie {..} uuid = do
    undefined
  --mac <- runDb $ getBy $ UniqueAccessKeyIdentifier uuid
  --case mac of
   -- Nothing -> throwAll err404 {errBody = "AccessKey not found."}
  --  Just (Entity _ ak) -> pure $ makeAccessKeyInfo ak

serveGetAccessKeys :: AuthCookie -> App [AccessKeyInfo]
serveGetAccessKeys AuthCookie {..} = do
    undefined
--  aks <- runDb $ selectList [AccessKeyUser ==. authCookieUserUUID] []
 -- pure $ map (makeAccessKeyInfo . entityVal) aks

-- Shoud use withPattern
servePostAddAccessKey :: AuthCookie -> AddAccessKey -> App AccessKeyCreated
servePostAddAccessKey AuthCookie {..} AddAccessKey {..} = undefined


withAuthResult :: WithError m => (AuthCookie -> m a) -> (AuthResult AuthCookie -> m a)
withAuthResult func ar = case ar of
    Authenticated ac -> func ac
    _                -> throwError $ ServerError "servantErr"

withAuthResultAndPermission :: WithError m => Permission -> (AuthCookie -> m a) -> (AuthResult AuthCookie -> m a)
withAuthResultAndPermission p func =
    withAuthResult (\ac -> withPermission (permissions ac) p =<< (func ac))

withPermission :: WithError m => [Permission] -> Permission -> a -> m a
withPermission ps p func =
    if elem p ps then return func else throwError $ ServerError "servantErr"


server :: AppEnv -> Server SiteApi
server env = hoistServerWithContext siteAPI
                                    (Proxy :: Proxy SiteContext)
                                    (runAppAsHandler env)
                                    (toServant siteServer)

type SiteContext = '[CookieSettings, JWTSettings]

docs :: Docs.API
docs = Docs.docs siteAPI

application :: AppEnv -> Application
application env = addPolicy
    $ serveWithContext siteAPI (siteContext env) (server env)
  where
    addPolicy = cors (const $ Just policy)
    policy    = simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods        = ["GET", "POST", "HEAD", "DELETE"]
        }


siteContext :: AppEnv -> Context SiteContext
siteContext Env {..} = cookieSettings :. jwtSettings :. EmptyContext

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
