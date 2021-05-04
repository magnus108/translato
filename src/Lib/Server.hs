module Lib.Server
    ( Api
    , application
    , api
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
                                                )


data TestAPI route = TestAPI
                        { p1 :: route
                                :- "foo"
                                :> Capture "i" Int
                                :> Get '[JSON] NoContent
                        , p2 :: route
                                :- "bar"
                                :> Get '[JSON] NoContent
                        } deriving (Generic)

instance Docs.ToCapture (Capture "i" Int) where
    toCapture _ = Docs.DocCapture "i" "No description"


testApiServer :: TestAPI AppServer
testApiServer =
    TestAPI { p1 = undefined -- loginHandler
            , p2 = undefined
            } -- logoutHandler


type Api = AuthApi :<|> (ToApi TestAPI)


api :: Proxy Api
api = Proxy

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    -- not save
    liftIO $ runApp env app



server :: AppEnv -> Server Api
server env = hoistServer
    api
    (runAppAsHandler env)
    (toServant authServer :<|> toServant testApiServer)

docs :: Docs.API
docs = Docs.docs api

application :: AppEnv -> Application
application env = serve api (server env)
