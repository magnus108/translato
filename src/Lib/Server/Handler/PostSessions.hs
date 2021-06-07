module Lib.Server.Handler.PostSessions where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Session
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson


servePostSessions :: AuthCookie -> Sessions -> ServerApp NoContent
servePostSessions authCookie sessions = do
    _ <- writeThing sessions =<< (grab @ServerApp.MSessionsFile)
    pure NoContent

