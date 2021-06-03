module Lib.Server.Handler.GetDoneshooting where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Doneshooting ( Doneshooting )

import qualified Lib.Server.Types                       as ServerApp

serveGetDoneshooting :: AuthCookie -> ServerApp Doneshooting
serveGetDoneshooting AuthCookie {..} = do
    doneshooting <- readDoneshooting
    pure $ doneshooting


type WithDoneshooting r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MDoneshootingFile r)


readDoneshooting :: forall  r m . WithDoneshooting r m => m Doneshooting
readDoneshooting = do
    mDoneshootingFile <-
        ServerApp.unMDoneshootingFile <$> grab @ServerApp.MDoneshootingFile
    doneshootingFile <- liftIO $ takeMVar mDoneshootingFile
    content           <-
        liftIO
        $         (readJSONFileStrict doneshootingFile)
        `finally` (putMVar mDoneshootingFile doneshootingFile)
    return content
