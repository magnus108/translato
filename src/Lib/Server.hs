module Lib.Server
    ( application
    , LoginForm
    )
where

import Lib.Server.Serve
import           Lib.Api
import           Lib.Api.Types
import           Lib.Client.Types
import           Lib.Data.Photographer          ( Photographers )

import           System.IO.Error
import           Control.Exception       hiding ( Handler )
import qualified Utils.ListZipper              as ListZipper

import           Blaze.ByteString.Builder       ( toByteString )
import           Lib.App.Error                  ( throwError
                                                , AppErrorType(..)
                                                , WithError(..)
                                                )

import           Lib.App.Error                  ( throwError
                                                , WithError
                                                )
import           Servant.API.Generic            ( toServant )
import           Servant.Server                 ( Application
                                                , Server
                                                , Handler
                                                , hoistServer
                                                , serve
                                                )

import qualified Lib.App                       as App
import           Lib.App                        ( AppEnv
                                                , AppError(..)
                                                , App
                                                , Env(..)
                                                , runApp
                                                )


import           Data.Aeson

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
                                                , PostNoContent(..)
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
                                                , genericApi
                                                , ToServant
                                                )



import           Network.Wai.Middleware.Cors
import           Data.UUID.Typed

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as LT
import qualified Text.Markdown                 as Markdown
import Lib.Server.Types
import Lib.Data.Permission




application :: ServerAppEnv -> Application
application env = addPolicy $ serveWithContext siteAPI (siteContext env) (server env)
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
