module Lib.Server
    ( Api
    , application
    , api
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
                                                )

type Api = AuthApi

api :: Proxy Api
api = Proxy

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    -- not save
    liftIO $ runApp env app

server :: AppEnv -> Server Api
server env =
    hoistServer api (runAppAsHandler env) (toServant authServer)


application :: AppEnv -> Application
application env = serve api (server env)
