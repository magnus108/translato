module Lib.Server.Handler.PostCameras where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Camera
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
    writeThing ServerApp.MCamerasFile cameras
    pure NoContent
