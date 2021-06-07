module Lib.Server.Handler.GetSessions where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Control.Exception              ( finally )
import           Lib.Data.Session               ( Sessions )

import qualified Lib.Server.Types              as ServerApp

serveGetSessions :: AuthCookie -> ServerApp Sessions
serveGetSessions AuthCookie {..} =
    readThing =<< (grab @ServerApp.MSessionsFile)
