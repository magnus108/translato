module Lib.Server.Handler.GetDagsdatoBackup where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.DagsdatoBackup ( DagsdatoBackup )

import qualified Lib.Server.Types                       as ServerApp

serveGetDagsdatoBackup :: AuthCookie -> ServerApp DagsdatoBackup
serveGetDagsdatoBackup AuthCookie {..} = do
    dagsdatoBackup <- readDagsdatoBackup
    pure $ dagsdatoBackup


type WithDagsdatoBackup r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MDagsdatoBackupFile r)


readDagsdatoBackup :: forall  r m . WithDagsdatoBackup r m => m DagsdatoBackup
readDagsdatoBackup = do
    mDagsdatoBackupFile <-
        ServerApp.unMDagsdatoBackupFile <$> grab @ServerApp.MDagsdatoBackupFile
    dagsdatoBackupFile <- liftIO $ takeMVar mDagsdatoBackupFile
    content           <-
        liftIO
        $         (readJSONFileStrict dagsdatoBackupFile)
        `finally` (putMVar mDagsdatoBackupFile dagsdatoBackupFile)
    return content
