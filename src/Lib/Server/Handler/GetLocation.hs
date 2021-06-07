module Lib.Server.Handler.GetLocation where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Control.Exception              ( finally )
import           Lib.Data.Location               ( Location )

import qualified Lib.Server.Types              as ServerApp

serveGetLocation :: AuthCookie -> ServerApp Location
serveGetLocation AuthCookie {..} =
    readThing =<< (grab @ServerApp.MLocationFile)
