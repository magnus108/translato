module Lib.Server.Handler.StreamDump where

import Lib.Api.Types
import Servant.Conduit
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Dump ( Dump )

import qualified Lib.Server.Types                       as ServerApp
import Control.Monad.Trans.Resource

import Servant.API.Stream
import Conduit
import Data.Conduit.FSNotify
import qualified System.FSNotify               as FS
import qualified System.FilePath               as FP
import Data.Acquire as A
import Control.Concurrent.Async
import qualified Servant.Types.SourceT as S
import Control.Concurrent hiding (readMVar)


serveStreamDump ::  ServerApp (SourceIO String)
serveStreamDump = main


main :: ServerApp (SourceIO String)
main = do
    mDumpFile <-
        ServerApp.unMDumpFile <$> grab @ServerApp.MDumpFile
    file <- liftIO $ readMVar mDumpFile
    let dir = FP.dropFileName file
    
    let fastSource = S.fromStepT mk where
        mk = S.Effect $ do
                traceShowM "lol"
                threadDelay 1000000
                return $ (S.Yield "lol" mk)

    pure $ show <$> fastSource
