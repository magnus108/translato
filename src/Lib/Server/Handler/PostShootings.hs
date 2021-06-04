module Lib.Server.Handler.PostShootings where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Shooting
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson


servePostShootings :: AuthCookie -> Shootings -> ServerApp NoContent
servePostShootings authCookie shootings = do
    _ <- writeThing shootings =<< (grab @ServerApp.MShootingsFile)
    pure NoContent
