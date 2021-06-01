module Lib.Server.Handler.GetDump where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Dump ( Dump )

import qualified Lib.Server.Types                       as ServerApp

serveGetDump :: AuthCookie -> ServerApp Dump
serveGetDump AuthCookie {..} = do
    dump <- readDump
    pure $ dump


type WithDump r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MDumpFile r)


readDump :: forall  r m . WithDump r m => m Dump
readDump = do
    mDumpFile <-
        ServerApp.unMDumpFile <$> grab @ServerApp.MDumpFile
    dumpFile <- liftIO $ takeMVar mDumpFile
    content           <-
        liftIO
        $         (readJSONFileStrict dumpFile)
        `finally` (putMVar mDumpFile dumpFile)
    return content
