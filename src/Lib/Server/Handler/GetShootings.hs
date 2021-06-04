module Lib.Server.Handler.GetShootings where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Control.Exception              ( finally )
import           Lib.Data.Shooting                ( Shootings )

import qualified Lib.Server.Types              as ServerApp

serveGetShootings :: AuthCookie -> ServerApp Shootings
serveGetShootings AuthCookie {..} = readThing =<< (grab @ServerApp.MShootingsFile)
