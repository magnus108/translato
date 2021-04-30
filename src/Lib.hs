module Lib
    ( mkAppEnv
    , runServer
    , main
    )
where

import           Control.Monad
import qualified Control.Concurrent.Async      as Async
import qualified Lib.Config                    as Config

import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , InChan(..)
                                                , OutChan(..)
                                                )

import           Network.Wai.Handler.Warp       ( run )
import           Lib.Server                     ( application )

import           Graphics.UI.Threepenny.Core

import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv port Config.Config {..} = do
    (inChan', outChan') <- Chan.newChan 200
    let inChan  = InChan inChan'
    let outChan = OutChan outChan'

    let static  = "static"
    let index   = "index.html"

    pure Env { .. }


runClient :: AppEnv -> IO ()
runClient env@Env {..} = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           }
        $ setup env


setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = return ()


runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
    run 8080 $ application env


main :: Int -> FilePath -> IO ()
main port root = do
    Config.loadConfig root "config.json"
        >>= mkAppEnv port
        >>= liftM2 Async.race_ runServer runClient
