module Lib.Server.Handler.GetDagsdato where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Dagsdato ( Dagsdato )

import qualified Lib.Server.Types                       as ServerApp

serveGetDagsdato :: AuthCookie -> ServerApp Dagsdato
serveGetDagsdato AuthCookie {..} = do
    dagsdato <- readDagsdato
    pure $ dagsdato


type WithDagsdato r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MDagsdatoFile r)


readDagsdato :: forall  r m . WithDagsdato r m => m Dagsdato
readDagsdato = do
    mDagsdatoFile <-
        ServerApp.unMDagsdatoFile <$> grab @ServerApp.MDagsdatoFile
    dagsdatoFile <- liftIO $ takeMVar mDagsdatoFile
    content           <-
        liftIO
        $         (readJSONFileStrict dagsdatoFile)
        `finally` (putMVar mDagsdatoFile dagsdatoFile)
    return content
