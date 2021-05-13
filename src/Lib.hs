module Lib
    ( mkAppEnv
    , runServer
    , main
    )
where


import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Servant.API
import           Control.Monad
import qualified Control.Concurrent.Async      as Async
import qualified Lib.Config                    as Config

import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , InChan(..)
                                                , OutChan(..)
                                                , runApp
                                                , runAppAsIO
                                                , App(..)
                                                , throwError
                                                , AppErrorType(..)
                                                )


import           Network.Wai.Handler.Warp       ( run )
import           Lib.Server                     ( application
                                                , docs
                                                )
import           Lib.Server.Auth
import qualified Servant.Docs                  as Docs

import           Graphics.UI.Threepenny.Core

import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           Servant.Client
import Servant.Auth.Server as Auth

import Crypto.JOSE.JWK (JWK)

mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv clientPort Config.Config {..} = do
    (inChan', outChan') <- Chan.newChan 200
    let inChan     = InChan inChan'
    let outChan    = OutChan outChan'

    let static     = "static"
    let index      = "index.html"

    let serverPort = 8080

    let baseUrl' = BaseUrl Http "localhost" serverPort ""
    manager' <- newManager defaultManagerSettings
    let cenv      = mkClientEnv manager' baseUrl'

    let cookieSettings = defaultCookieSettings { cookieIsSecure    = NotSecure}
    signingKey <- loadSigningKey -- serveSetSigningKeyFile
    let jwtSettings =  defaultJWTSettings signingKey

    pure Env { .. }

loadSigningKey :: IO JWK
loadSigningKey = do
    key <- Auth.generateKey
    pure key


runClient :: AppEnv -> IO ()
runClient env@Env {..} = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just clientPort
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           }
        $ setup env


setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = do
    -- config
    --let ff :<|> ss :<|> dd = hoistClient api (runClientM' cenv) (client api)
    -- liftIO $ runAppAsIO env (ss 1)

-------------------------------------------------------------------------------
    return win # set title "test"

    return ()

runClientM' :: ClientEnv -> ClientM a -> App a
runClientM' clietEnv client = do
    e <- liftIO $ runClientM client clietEnv
    case e of
        Left  servantErr -> throwError $ ServerError "servantErr" -- servantErr
        Right a          -> pure a


runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
    putStrLn $ Docs.markdown docs
    run serverPort $ application env


main :: Int -> FilePath -> IO ()
main port root = do
    Config.loadConfig root "config/config.json"
        >>= mkAppEnv port
        >>= liftM2 Async.race_ runServer runClient
