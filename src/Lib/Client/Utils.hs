module Lib.Client.Utils where

import qualified Foreign.JavaScript              as JS
import           Servant.Auth.Client
import           Control.Conditional            ( (?<>) )
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Lib.Client.Types
import           Lib.Utils
import           Servant                 hiding ( Handler )

import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import           Web.Cookie                     ( parseSetCookie
                                                , setCookieName
                                                , SetCookie(..)
                                                , setCookieValue
                                                )

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )

import           Servant.Auth.Client
import           Lib.Data.Photographer          ( Photographers )
import           Graphics.UI.Threepenny.Core
import           Servant                 hiding ( throwError
                                                , Handler
                                                )
import qualified Servant.Client                as Servant

import           Lib.Client.Error               ( ClientAppError
                                                , throwError
                                                , ClientAppErrorType(..)
                                                )
import           GHC.Base                       ( failIO )

import qualified Control.Monad.Except          as E

import           Lib.Utils
import           Lib.Api.Types
import           Lib.Api                 hiding ( GetPhotographers
                                                , PostTabs
                                                , GetTabs
                                                )
import qualified Utils.ListZipper              as ListZipper
import qualified Control.Lens                  as Lens
import qualified Lib.Data.Photographer         as Photographer
import qualified Lib.Data.Tab                  as Tab
import           Control.Comonad

onEvent' :: Event a -> (a -> ClientApp void) -> ClientApp (ClientApp ())
onEvent' e h = do
    env                 <- ask
    window <- liftUI $ askWindow
    let flush = liftUI $ liftJSWindow $ \w -> do
            mode <- JS.getCallBufferMode w
            case mode of
                FlushOften -> JS.flushCallBuffer w
                _          -> return ()
    unregister <- liftIO $ register e (void . runUI window . runClientApp env . (>> flush) . h)
    return (liftIO unregister)

toItem :: Show a => a -> UI Element
toItem a = UI.string (show a)

items :: Show a => ReadWriteAttr Element [a] ()
items = mkWriteAttr $ \is x -> void $ do
    return x # set children [] #+ fmap (\i -> UI.string (show i)) is

items' :: ReadWriteAttr Element [UI Element] ()
items' = mkWriteAttr $ \is x -> void $ do
    is' <- sequence is
    return x # set children is'


electronDialog :: [String] -> JS.JSObject -> JSFunction ()
electronDialog options callback = ffi
    "require('electron').remote.dialog.showOpenDialog({properties: %2}).then(result => %1(result.filePaths[0]))"
    callback
    options
