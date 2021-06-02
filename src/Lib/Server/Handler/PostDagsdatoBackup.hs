module Lib.Server.Handler.PostDagsdatoBackup where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.DagsdatoBackup ( DagsdatoBackup )
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostDagsdatoBackup :: AuthCookie -> DagsdatoBackup -> ServerApp NoContent
servePostDagsdatoBackup authCookie dagsdatoBackup = do
    _ <- writeDagsdatoBackup dagsdatoBackup
    pure NoContent



type WithDagsdatoBackup r m = (MonadReader r m, MonadIO m, Has ServerApp.MDagsdatoBackupFile r)


writeDagsdatoBackup :: forall  r m . WithDagsdatoBackup r m => DagsdatoBackup -> m ()
writeDagsdatoBackup dagsdatoBackup = do
    mDagsdatoBackupFile <- ServerApp.unMDagsdatoBackupFile <$> grab @ServerApp.MDagsdatoBackupFile
    dagsdatoBackupFile  <- liftIO $ takeMVar mDagsdatoBackupFile
    liftIO $ writeJSONFile dagsdatoBackupFile dagsdatoBackup `finally` (putMVar mDagsdatoBackupFile dagsdatoBackupFile)

