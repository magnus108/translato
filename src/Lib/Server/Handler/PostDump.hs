module Lib.Server.Handler.PostDump where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Dump ( Dump )
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostDump :: AuthCookie -> Dump -> ServerApp NoContent
servePostDump authCookie dump = do
    _ <- writeDump dump
    pure NoContent



type WithDump r m = (MonadReader r m, MonadIO m, Has ServerApp.MDumpFile r)


writeDump :: forall  r m . WithDump r m => Dump -> m ()
writeDump dump = do
    mDumpFile <- ServerApp.unMDumpFile <$> grab @ServerApp.MDumpFile
    dumpFile  <- liftIO $ takeMVar mDumpFile
    liftIO $ writeJSONFile dumpFile dump `finally` (putMVar mDumpFile dumpFile)

