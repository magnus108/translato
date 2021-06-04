module Lib.Server.Handler.GetCameras where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Camera          ( Cameras )

import qualified Lib.Server.Types                       as ServerApp

serveGetCameras :: AuthCookie -> ServerApp Cameras
serveGetCameras AuthCookie {..} = readThing =<< (grab @ServerApp.MCamerasFile)
