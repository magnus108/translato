module Lib.Server.Handler.GetPhotographers where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Photographer          ( Photographers )

import qualified Lib.Server.Types                       as ServerApp

serveGetPhotographers :: AuthCookie -> ServerApp Photographers
serveGetPhotographers AuthCookie {..} = do
    photographers <- readPhotographers
    pure $ photographers


type WithPhotographers r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MPhotographersFile r)


readPhotographers :: forall  r m . WithPhotographers r m => m Photographers
readPhotographers = do
    mPhotographersFile <-
        ServerApp.unMPhotographersFile <$> grab @ServerApp.MPhotographersFile
    photographersFile <- liftIO $ takeMVar mPhotographersFile
    content           <-
        liftIO
        $         (readJSONFileStrict photographersFile)
        `finally` (putMVar mPhotographersFile photographersFile)
    return content
