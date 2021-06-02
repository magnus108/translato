module Lib.Server.Handler.PostDagsdato where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Dagsdato ( Dagsdato )
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostDagsdato :: AuthCookie -> Dagsdato -> ServerApp NoContent
servePostDagsdato authCookie dagsdato = do
    _ <- writeDagsdato dagsdato
    pure NoContent



type WithDagsdato r m = (MonadReader r m, MonadIO m, Has ServerApp.MDagsdatoFile r)


writeDagsdato :: forall  r m . WithDagsdato r m => Dagsdato -> m ()
writeDagsdato dagsdato = do
    mDagsdatoFile <- ServerApp.unMDagsdatoFile <$> grab @ServerApp.MDagsdatoFile
    dagsdatoFile  <- liftIO $ takeMVar mDagsdatoFile
    liftIO $ writeJSONFile dagsdatoFile dagsdato `finally` (putMVar mDagsdatoFile dagsdatoFile)

