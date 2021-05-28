module Lib.Server.Handler.PostTabs where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Tab
import qualified Lib.Server.Types              as ServerApp
import           Servant (NoContent(..))

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

servePostTabs :: AuthCookie -> Tabs -> ServerApp NoContent
servePostTabs authCookie tabs = do
    pure NoContent

