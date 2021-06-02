{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

module Lib.Client.FilePicker where
import qualified Foreign.JavaScript            as JS
import qualified Relude.Unsafe as Unsafe
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
                                                , PostPhotographers
                                                )
import qualified Utils.ListZipper              as ListZipper
import qualified Control.Lens                  as Lens
import qualified Lib.Data.Photographer         as Photographer
import qualified Lib.Data.Tab                  as Tab
import           Control.Comonad hiding ((<@))
import           Lib.Client.Utils


mkFilePicker :: EDialog -> Behavior (Maybe FilePath) -> (FilePath -> UI Element) -> UI (Element, Event (Maybe FilePath))
mkFilePicker (EDialog eDialog) bItems showItem = mdo
    (eChange, hChange) <- liftIO $ newEvent
    (eElectronDialog, hElectronDialog) <- liftIO $ newEvent

    let electronFileHandler handler file = when (file /= "") $ handler (Just file)

    callback <- liftUI $ ffiExport (electronFileHandler hChange)

    let bErrorDisplay = pure $ UI.string "no text"

    let bDisplay = pure $ \fp -> do
            display <-
                UI.button
                #. "button"
                #+ [showItem fp, UI.span #. "icon" #+ [UI.mkElement "i" #. "far fa-file"]]

            UI.on UI.click display $ \_ -> do
                liftIO $ hElectronDialog ()

            return display

    _ <- onEvent eElectronDialog $ \_ -> do
        eDialog ["openDirectory"] callback

    let display bItem = do
            item <- bItem
            display <- bDisplay
            errorDisplay <- bErrorDisplay
            return $ case item of
                       Nothing -> [errorDisplay]
                       Just item ->  [display item]


    item <- liftUI $ UI.div # sink items' (display bItems)

    return (item, eChange)



