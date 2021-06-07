module Lib.Server.Handler.GetGrades where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Control.Exception              ( finally )
import           Lib.Data.Grade                 ( Grades )

import qualified Lib.Server.Types              as ServerApp

serveGetGrades :: AuthCookie -> ServerApp Grades
serveGetGrades AuthCookie {..} = readThing =<< (grab @ServerApp.MGradesFile)
