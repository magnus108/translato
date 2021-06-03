module Lib.Server.Handler.PostCameras where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Camera
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostCameras :: AuthCookie -> Cameras -> ServerApp NoContent
servePostCameras authCookie cameras = do
    _ <- writeCameras cameras
    pure NoContent



type WithCameras r m = (MonadReader r m, MonadIO m, Has ServerApp.MCamerasFile r)


writeCameras :: forall  r m . WithCameras r m => Cameras -> m ()
writeCameras cameras = do
    mCamerasFile <- ServerApp.unMCamerasFile <$> grab @ServerApp.MCamerasFile
    camerasFile  <- liftIO $ takeMVar mCamerasFile
    liftIO $ writeJSONFile camerasFile cameras `finally` (putMVar mCamerasFile camerasFile)

