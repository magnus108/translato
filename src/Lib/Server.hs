module Lib.Server
    ( application
    , LoginForm
    )
where

import           Lib.Server.Serve
import           Lib.Api
import           Lib.Api.Types

import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           Servant.Auth.Server
import           Servant.Server.Generic         ( genericServerT )

import           Network.Wai.Middleware.Cors

import           Lib.Server.Types




application :: ServerAppEnv -> Application
application env = addPolicy
    $ serveWithContext siteAPI (siteContext env) (server env)
  where
    addPolicy = cors (const $ Just policy)
    policy    = simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods        = ["GET", "POST", "HEAD", "DELETE"]
        }


server :: ServerAppEnv -> Server SiteApi
server env = hoistServerWithContext siteAPI
                                    (Proxy :: Proxy SiteContext)
                                    (runServerAppAsHandler env)
                                    (genericServerT siteServer)


runServerAppAsHandler :: ServerAppEnv -> ServerApp a -> Handler a
runServerAppAsHandler env app = do
    -- not save
    liftIO $ runServerApp env app


siteContext :: ServerAppEnv -> Context SiteContext
siteContext ServerEnv {..} = cookieSettings :. jwtSettings :. EmptyContext

type SiteContext = '[CookieSettings, JWTSettings]
