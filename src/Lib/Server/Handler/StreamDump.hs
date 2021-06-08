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
    
    -- MOSTLIKELY NOT SAFE!!!
    ---let thingy = (setPredicate (\e -> FP.takeFileName (FS.eventPath e) == file)  $ setRelative True $ mkFileChangeSettings (traceShow dir dir))
    traceShowM "ggggg1123"
--    hip <- liftIO $ with (acquireSourceFileChanges (setRelative True $ mkFileChangeSettings dir) :: Acquire (ConduitM i Event IO ())) (\x -> return (conduitToSourceIO x))

    traceShowM "ggg1235"

    {-
    let fastSource = S.fromStepT . mk where
        mk m
            | m < 0     = S.Stop
            | otherwise = S.Yield m (mk (m - 1))

    let slowSource m = S.mapStepT delay (fastSource m) where
        delay S.Stop        = S.Stop
        delay (S.Error err) = S.Error err
        delay (S.Skip s)    = S.Skip (delay s)
        delay (S.Effect ms) = S.Effect (fmap delay ms)
        delay (S.Yield x s) = S.Effect $
            S.Yield x (delay s) <$ threadDelay 1000000
-}

    let fastSource = S.fromStepT mk where
        mk = S.Effect $ do
                traceShowM "lol"
                threadDelay 1000000
                return $ (S.Yield "lol" mk)

    {-
    let sourcy = S.mapStepT delayIt hip where
        delayIt S.Stop        = S.Stop
        delayIt (S.Error err) = S.Error err
        delayIt (S.Skip s)    = S.Skip delayIt
        delayIt (S.Effect ms) = S.Effect (fmap delayIt ms)
        delayIt (S.Yield x s) = S.Effect $
            S.Yield x (delayIt s)
            --S.Yield x (delayIt s) <$ threadDelay 1000000
            ---}

    pure $ show <$> fastSource
        {-
    let dingy = conduitToSourceIO $ sourceFileChanges (setRelative True $ mkFileChangeSettings dir) $$ mapM_C (liftIO . print)

    gggMax <- liftIO $ runResourceT $ resourceForkWith (\x -> do
                    eh <- withAsync x $ \ s -> do 
                        traceShowM "wasssssssser"
                        res <- wait s
                        traceShowM "wasssss"
                        return res
                    traceShowM "CASS"
                    return eh
                ) $ (sourceFileChanges (setRelative True $ mkFileChangeSettings dir) $$ mapM_C (liftIO . print) :: ResourceT IO ())
    let xx = conduitToSourceIO (yield gggMax .| mapC show :: ConduitT i String IO ())
    traceShowM "FUCKERS ARE FUCKED"
    -}
