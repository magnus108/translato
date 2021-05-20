module Lib.Server.Handler.GetPermissions where

import Lib.Data.Permission
import Lib.Api.Types
import Lib.Server.Types

serveGetPermissions :: AuthCookie -> ServerApp [Permission]
serveGetPermissions AuthCookie {..} = pure permissions
