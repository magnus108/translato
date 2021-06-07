module Lib.Server.Handler.PostGrades where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Grade
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson


servePostGrades :: AuthCookie -> Grades -> ServerApp NoContent
servePostGrades authCookie grades = do
    _ <- writeThing grades =<< (grab @ServerApp.MGradesFile)
    pure NoContent

