module Lib
    ( main
    )
where


import           Lib.Client
import           Lib.Client.Types
import           Servant.API
import           Control.Monad
import qualified Control.Concurrent.Async      as Async
import qualified Lib.Config                    as Config

import qualified Lib.Server.Types              as ServerApp
import qualified Lib.App                       as App
import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                )

import           Network.Wai.Handler.Warp       ( run )
import           Lib.Server                     ( application )


import           Servant.Auth.Server           as Auth

import           Crypto.JOSE.JWK                ( JWK )

import           Graphics.UI.Threepenny.Core


mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv clientPort Config.Config {..} = do
    mPhotographersFile' <- newMVar photographersFile
    let mPhotographersFile = App.MPhotographersFile mPhotographersFile'

    let serverPort         = 8080

    let static             = "static"
    let index              = "index.html"

    pure Env { .. }


mkServerAppEnv :: AppEnv -> IO ServerApp.ServerAppEnv
mkServerAppEnv Env {..} = do
    let unMPhotographersFile = App.unMPhotographersFile mPhotographersFile
    let mPhotographersFile   = ServerApp.MPhotographersFile unMPhotographersFile

    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
    signingKey <- loadSigningKey -- serveSetSigningKeyFile
    let jwtSettings = defaultJWTSettings signingKey

    pure ServerApp.ServerEnv { .. }


loadSigningKey :: IO JWK
loadSigningKey = do
    key <- Auth.generateKey
    pure key


runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
    serverEnv <- mkServerAppEnv env
    run serverPort $ application serverEnv


runClient :: AppEnv -> IO ()
runClient env@Env {..} = do
    clientEnv <- mkClientAppEnv
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just clientPort
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           }
        $ runClientApp clientEnv
        . setup

main :: Int -> FilePath -> IO ()
main port root = do
    Config.loadConfig root "config/config.json"
        >>= mkAppEnv port
        >>= liftM2 Async.race_ runServer runClient
