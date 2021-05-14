{-# LANGUAGE GADTs #-}
module Lib
    ( mkAppEnv
    , runServer
    , main
    )
where



import Servant.Auth.Client

import Web.Cookie (parseSetCookie, setCookieName, setCookieValue)

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Servant.API
import           Control.Monad
import qualified Control.Concurrent.Async      as Async
import qualified Lib.Config                    as Config

import qualified Lib.App                        as App
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
import qualified Data.Text as T
import qualified Servant.Docs                  as Docs

import           Graphics.UI.Threepenny.Core

import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           Servant.Client
import           Servant.Auth.Server           as Auth

import           Crypto.JOSE.JWK                ( JWK )

import qualified Lib.Client as Client

mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv clientPort Config.Config {..} = do
    (inChan', outChan') <- Chan.newChan 200
    let inChan     = InChan inChan'
    let outChan    = OutChan outChan'


    mPhotographersFile' <- newMVar photographersFile
    let mPhotographersFile = App.MPhotographersFile mPhotographersFile'

    let static     = "static"
    let index      = "index.html"

    let serverPort = 8080

    let baseUrl' = BaseUrl Http "localhost" serverPort ""
    manager' <- newManager defaultManagerSettings
    let cenv           = mkClientEnv manager' baseUrl'
    let clients = Client.clients cenv

    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
    signingKey <- loadSigningKey -- serveSetSigningKeyFile
    let jwtSettings = defaultJWTSettings signingKey

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

-------------------------------------------------------------------------------

clientGG :: Window -> ClientApp ()
clientGG win = do
    liftUI (return win # set title "test")
    return ()


runClientGG :: AppEnv -> IO ()
runClientGG env@Env {..} = do 
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just clientPort
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           }
        $ runClientApp ClientEnvv . clientGG

type ClientAppEnv = ClientEnvv ClientApp

newtype ClientApp a = ClientApp
    { unClientApp :: ReaderT ClientAppEnv UI a
    } deriving newtype ( Functor
                        , Applicative
                        , Monad
                        , MonadIO
                        , MonadReader ClientAppEnv
                        )

instance MonadUI ClientApp where
    liftUI m = ClientApp (ReaderT (const m))

runClientApp :: ClientAppEnv -> ClientApp a -> UI a
runClientApp env = usingReaderT env . unClientApp

data ClientEnvv (m :: Type -> Type) = ClientEnvv

-------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = do
    gg <- liftIO $ runAppAsIO env $ (App.login clients) (App.LoginForm (App.Username "bob") "tste")
    --- https://github.com/NorfairKing/intray/blob/f8ed32c79d8b3159a4c82cd8cc5c5aeeb92c2c90/intray-cli/src/Intray/Cli/Commands/Login.hs
    cook <- case gg of
            Left _  -> error "No server configured."
            Right (Headers NoContent (HCons sessionHeader HNil)) ->
                case sessionHeader of
                    MissingHeader -> error "The server responded but the response was missing the right session header."
                    UndecodableHeader _ -> error "The server responded but the response had an undecodable session header."
                    Header setCookieText -> do
                        let cookies = parseSetCookie . encodeUtf8 <$> T.lines setCookieText
                        let jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
                        case jwtCookie of
                                Nothing -> error "No JWT-Cookie was found in the Set-Cookie session header."
                                Just setCookie -> do
                                    traceShowM setCookie
                                    --saveSession setCook
                                    return $ setCookie

    let token = Token (setCookieValue  cook)
    photographers <- liftIO $ runAppAsIO env $ (App.getPhotographers' clients) token
    traceShowM photographers

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
        >>= liftM2 Async.race_ runServer runClientGG
