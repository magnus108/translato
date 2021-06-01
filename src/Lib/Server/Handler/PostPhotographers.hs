module Lib.Server.Handler.PostPhotographers where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Photographer
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostPhotographers :: AuthCookie -> Photographers -> ServerApp NoContent
servePostPhotographers authCookie photographers = do
    _ <- writePhotographers photographers
    pure NoContent



type WithPhotographers r m = (MonadReader r m, MonadIO m, Has ServerApp.MPhotographersFile r)


writePhotographers :: forall  r m . WithPhotographers r m => Photographers -> m ()
writePhotographers photographers = do
    mPhotographersFile <- ServerApp.unMPhotographersFile <$> grab @ServerApp.MPhotographersFile
    photographersFile  <- liftIO $ takeMVar mPhotographersFile
    liftIO $ writeJSONFile photographersFile photographers `finally` (putMVar mPhotographersFile photographersFile)
