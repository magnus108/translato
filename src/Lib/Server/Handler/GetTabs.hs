module Lib.Server.Handler.GetTabs where

import Lib.Api.Types
import Lib.Server.Types
import Lib.Utils
import           Control.Exception (finally)
import           Lib.Data.Tab          ( Tabs )

import qualified Lib.Server.Types                       as ServerApp

serveGetTabs :: AuthCookie -> ServerApp Tabs
serveGetTabs AuthCookie {..} = do
    tabs <- readTabs
    pure $ tabs


type WithTabs r m
    = (MonadReader r m, MonadIO m, Has ServerApp.MTabsFile r)


readTabs :: forall  r m . WithTabs r m => m Tabs
readTabs = do
    mTabsFile <-
        ServerApp.unMTabsFile <$> grab @ServerApp.MTabsFile
    tabsFile <- liftIO $ takeMVar mTabsFile
    content           <-
        liftIO
        $         (readJSONFileStrict tabsFile)
        `finally` (putMVar mTabsFile tabsFile)
    return content
