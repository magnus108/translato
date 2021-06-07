module Lib.Server.Handler.PostLocation where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Location
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson


servePostLocation :: AuthCookie -> Location -> ServerApp NoContent
servePostLocation authCookie location = do
    _ <- writeThing location =<< (grab @ServerApp.MLocationFile)
    pure NoContent

