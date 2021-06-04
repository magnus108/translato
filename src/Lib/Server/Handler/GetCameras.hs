module Lib.Server.Handler.GetCameras where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Camera          ( Cameras )

import qualified Lib.Server.Types                       as ServerApp

serveGetCameras :: AuthCookie -> ServerApp Cameras
serveGetCameras AuthCookie {..} = do
    cameras <- readCameras
    pure $ cameras


type WithCameras r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MCamerasFile r)


readCameras :: forall  r m . WithCameras r m => m Cameras
readCameras = do
    mCamerasFile <-
        ServerApp.unMCamerasFile <$> grab @ServerApp.MCamerasFile
    camerasFile <- liftIO $ takeMVar mCamerasFile
    content           <-
        liftIO
        $         (readJSONFileStrict camerasFile)
        `finally` (putMVar mCamerasFile camerasFile)
    return content
