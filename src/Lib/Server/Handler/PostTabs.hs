module Lib.Server.Handler.PostTabs where

import           Lib.Api.Types
import           Lib.Server.Types
import           Lib.Utils
import           Lib.Data.Tab
import           Control.Exception              ( finally )
import qualified Lib.Server.Types              as ServerApp
import           Servant                        ( NoContent(..) )

import           Lib.Server.Error               ( throwError
                                                , WithError
                                                , ServerAppErrorType(..)
                                                )

import qualified Data.ByteString.Lazy          as BS
import           Data.Aeson

servePostTabs :: AuthCookie -> Tabs -> ServerApp NoContent
servePostTabs authCookie tabs = do
    _ <- writeTabs tabs
    pure NoContent



type WithTabs r m = (MonadReader r m, MonadIO m, Has ServerApp.MTabsFile r)


writeTabs :: forall  r m . WithTabs r m => Tabs -> m ()
writeTabs tabs = do
    mTabsFile <- ServerApp.unMTabsFile <$> grab @ServerApp.MTabsFile
    tabsFile  <- liftIO $ takeMVar mTabsFile
    liftIO $ writeJSONFile tabsFile tabs `finally` (putMVar mTabsFile tabsFile)

