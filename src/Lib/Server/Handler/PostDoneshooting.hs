module Lib.Server.Handler.PostDoneshooting where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Doneshooting ( Doneshooting )
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostDoneshooting :: AuthCookie -> Doneshooting -> ServerApp NoContent
servePostDoneshooting authCookie doneshooting = do
    _ <- writeDoneshooting doneshooting
    pure NoContent



type WithDoneshooting r m = (MonadReader r m, MonadIO m, Has ServerApp.MDoneshootingFile r)


writeDoneshooting :: forall  r m . WithDoneshooting r m => Doneshooting -> m ()
writeDoneshooting doneshooting = do
    mDoneshootingFile <- ServerApp.unMDoneshootingFile <$> grab @ServerApp.MDoneshootingFile
    doneshootingFile  <- liftIO $ takeMVar mDoneshootingFile
    liftIO $ writeJSONFile doneshootingFile doneshooting `finally` (putMVar mDoneshootingFile doneshootingFile)

