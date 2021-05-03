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
                                                )

import qualified Servant.Docs                  as Docs

type Api = AuthApi

api :: Proxy Api
api = Proxy

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    -- not save
    liftIO $ runApp env app

server :: AppEnv -> Server Api
server env = hoistServer api (runAppAsHandler env) (toServant authServer)

docs :: Docs.API
docs = Docs.docs api

application :: AppEnv -> Application
application env = serve api (server env)
