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

    mTabsFile' <- newMVar tabsFile
    let mTabsFile = App.MTabsFile mTabsFile'

    mCamerasFile' <- newMVar camerasFile
    let mCamerasFile = App.MCamerasFile mCamerasFile'

    mDumpFile' <- newMVar dumpFile
    let mDumpFile = App.MDumpFile mDumpFile'

    mDagsdatoFile' <- newMVar dagsdatoFile
    let mDagsdatoFile = App.MDagsdatoFile mDagsdatoFile'

    mDoneshootingFile' <- newMVar doneshootingFile
    let mDoneshootingFile = App.MDoneshootingFile mDoneshootingFile'

    mDagsdatoBackupFile' <- newMVar dagsdatoBackupFile
    let mDagsdatoBackupFile = App.MDagsdatoBackupFile mDagsdatoBackupFile'

    let serverPort         = 8080

    let static             = "static"
    let index              = "index.html"

    pure Env { .. }


mkServerAppEnv :: AppEnv -> IO ServerApp.ServerAppEnv
mkServerAppEnv Env {..} = do
    let unMPhotographersFile = App.unMPhotographersFile mPhotographersFile
    let mPhotographersFile   = ServerApp.MPhotographersFile unMPhotographersFile

    let unMTabsFile = App.unMTabsFile mTabsFile
    let mTabsFile   = ServerApp.MTabsFile unMTabsFile

    let unMCamerasFile = App.unMCamerasFile mCamerasFile
    let mCamerasFile   = ServerApp.MCamerasFile unMCamerasFile

    let unMDumpFile = App.unMDumpFile mDumpFile
    let mDumpFile   = ServerApp.MDumpFile unMDumpFile

    let unMDagsdatoFile = App.unMDagsdatoFile mDagsdatoFile
    let mDagsdatoFile   = ServerApp.MDagsdatoFile unMDagsdatoFile

    let unMDoneshootingFile = App.unMDoneshootingFile mDoneshootingFile
    let mDoneshootingFile   = ServerApp.MDoneshootingFile unMDoneshootingFile

    let unMDagsdatoBackupFile = App.unMDagsdatoBackupFile mDagsdatoBackupFile
    let mDagsdatoBackupFile   = ServerApp.MDagsdatoBackupFile unMDagsdatoBackupFile


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
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just clientPort
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           }
        $ (\win -> do 
                -- actually wrong. should use queue, because ioref
                clientEnv <- liftIO $ mkClientAppEnv
                runClientApp clientEnv (setup win)
          )

main :: Int -> FilePath -> IO ()
main port root = do
    Config.loadConfig root "config/config.json"
        >>= mkAppEnv port
        >>= liftM2 Async.race_ runServer runClient
