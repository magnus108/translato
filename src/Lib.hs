{-# LANGUAGE GADTs #-}
module Lib
    ( mkAppEnv
    , runServer
    , main
    )
where

import Lib.Utils
import Lib.Client.Types


import Servant.Auth.Client

import Web.Cookie (parseSetCookie, setCookieName, setCookieValue)

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Lib.Data.Photographer          ( Photographers(..))
import           Servant.API
import           Control.Monad
import qualified Control.Concurrent.Async      as Async
import qualified Lib.Config                    as Config

import qualified Lib.Server.Types                 as ServerApp
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
                                                )
import qualified Data.Text as T
import qualified Servant.Docs                  as Docs

import           Graphics.UI.Threepenny.Core

import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           qualified Servant.Client as ServantClient
import           Servant.Client hiding (ClientEnv)
import           Servant.Auth.Server           as Auth

import           Crypto.JOSE.JWK                ( JWK )


mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv clientPort Config.Config {..} = do
    (inChan', outChan') <- Chan.newChan 200
    let inChan     = InChan inChan'
    let outChan    = OutChan outChan'


    mPhotographersFile' <- newMVar photographersFile
    let mPhotographersFile = App.MPhotographersFile mPhotographersFile'

    let serverPort = 8080

    let static     = "static"
    let index      = "index.html"


    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
    signingKey <- loadSigningKey -- serveSetSigningKeyFile
    let jwtSettings = defaultJWTSettings signingKey

    pure Env { .. }

mkServerAppEnv :: AppEnv -> IO ServerApp.ServerAppEnv
mkServerAppEnv Env{..}= do
    let unMPhotographersFile = App.unMPhotographersFile mPhotographersFile
    let mPhotographersFile = ServerApp.MPhotographersFile unMPhotographersFile 

    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
    signingKey <- loadSigningKey -- serveSetSigningKeyFile
    let jwtSettings = defaultJWTSettings signingKey

    pure ServerApp.ServerEnv { .. }

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
    clientEnv <- mkClientAppEnv
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just clientPort
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           }
        $ runClientApp clientEnv . clientGG

-------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = do
    {-
    gg <- liftIO $ runAppAsIO env $ (login clients) (LoginForm (Username "bob") "tste")
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
    photographers <- liftIO $ runAppAsIO env $ (getPhotographers' clients) token
    traceShowM photographers
-}
-------------------------------------------------------------------------------
    return win # set title "test"

    return ()


runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
    serverEnv <- mkServerAppEnv env
    run serverPort $ application serverEnv


main :: Int -> FilePath -> IO ()
main port root = do
    Config.loadConfig root "config/config.json"
        >>= mkAppEnv port
        >>= liftM2 Async.race_ runServer runClientGG
