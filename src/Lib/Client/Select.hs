{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

module Lib.Client.Select where
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


data Mode
    = Closed
    | Open
    deriving (Eq, Show)


switch :: Mode -> Mode
switch Open = Closed
switch Closed = Open


mkSelect :: Behavior (Maybe (ListZipper.ListZipper a)) -> (a -> UI Element) -> UI (Element, Event (Maybe (ListZipper.ListZipper a)))
mkSelect bItems showItem = mdo

    (eSelection, hSelection) <- liftIO $ newEvent
    (ePopup , hPopup      ) <- liftIO $ newEvent

    let eSwitch = switch <$> Unsafe.head <$> unions
            [ bDropMode <@ eSelection
            , bDropMode <@ ePopup
            ]

    bDropMode <- stepper Closed $ eSwitch

    let bDisplayOpen = pure $ \center items' -> do
            display <-
                UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                #+ [showItem (extract items')]

            UI.on UI.click display $ \_ -> do
                liftIO $ hSelection (Just items')

            return display


    let bDisplayClosed = pure $ \items' -> do
            display <-
                UI.button
                #. "button"
                #+ [ UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
                   , showItem (extract items')
                   ]

            UI.on UI.click display $ \_ -> do
                    liftIO $ hPopup ()

            return display

    let errorDisplay = UI.string "no text"

    let display bItems = do
            displayOpen <- bDisplayOpen
            displayClosed <- bDisplayClosed
            dropMode <- bDropMode
            items <- bItems
            return $ case items of
                       Nothing -> [errorDisplay]
                       Just zipper -> case dropMode of
                            Open -> do
                                [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend displayOpen zipper)]
                            Closed -> do
                                [displayClosed zipper]


    item <- liftUI $ UI.div # sink items' (display bItems)

    return (item, eSelection)

