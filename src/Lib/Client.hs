{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

module Lib.Client where

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


mkTabs :: ClientApp (Element, Event (Maybe Tab.Tabs))
mkTabs = do
    (BTabs bTabs)            <- grab @BTabs

    (eSelection, hSelection) <- liftIO $ newEvent

    let errorDisplay = UI.string "no text"
    let showTab x = UI.string $ show x
    let display center tabs = do
            display <-
                UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                #+ [showTab (extract tabs)]

            UI.on UI.click display $ \_ -> do
                liftIO $ hSelection (Just (Tab.Tabs tabs))

            return display

    item <- liftUI $ UI.div #. "buttons has-addons" # sink
        items'
        (   fromMaybe [errorDisplay]
        <$> (fmap
                (ListZipper.toList . ListZipper.bextend display . Tab.unTabs)
            )
        <$> bTabs
        )

    return (item, eSelection)

mkToken = do
    (BToken bToken) <- grab @BToken
    liftUI $ UI.p # sink
        text
        (show <$> (fromMaybe (B.empty)) <$> (fmap setCookieValue) <$> bToken)



--------------------------------------------------------------------------------

data Mode
    = Closed
    | Open
    deriving (Eq, Show)


switch :: Mode -> Mode
switch Open = Closed
switch Closed = Open



mkPhotographersTab :: ClientApp (Element, Event (Maybe Photographer.Photographers))
mkPhotographersTab = mdo
    (BPhotographers bPhotographers)        <- grab @BPhotographers

    (eSelection, hSelection) <- liftIO $ newEvent
    (ePopup , hPopup      ) <- liftIO $ newEvent

    let eSwitch = switch <$> Unsafe.head <$> unions
            [ bDropMode <@ eSelection
            , bDropMode <@ ePopup
            ]

    bDropMode <- stepper Closed $ eSwitch

    let showPhotographer x = UI.string $ T.unpack $ Photographer.name x

    let bDisplayOpen = pure $ \center photographers -> do
            display <-
                UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                #+ [showPhotographer (extract photographers)]

            UI.on UI.click display $ \_ -> do
                liftIO $ hSelection (Just (Photographer.Photographers photographers))

            return display


    let bDisplayClosed = pure $ \photographers -> do
            display <-
                UI.button
                #. "button"
                #+ [ UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
                   , showPhotographer (extract photographers)
                   ]

            UI.on UI.click display $ \_ -> do
                    liftIO $ hPopup ()

            return display

    let errorDisplay = UI.string "no text"

    let display bPhotographers = do
            displayOpen <- bDisplayOpen
            displayClosed <- bDisplayClosed
            dropMode <- bDropMode
            photographers <- bPhotographers
            return $ case photographers of
                       Nothing -> [errorDisplay]
                       Just (Photographer.Photographers zipper) -> case dropMode of
                            Open -> do
                                [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend displayOpen zipper)]
                            Closed -> do
                                [displayClosed zipper]


    item <- liftUI $ UI.div # sink items' (display bPhotographers)

    return (item, eSelection)

--------------------------------------------------------------------------------

mkContent :: ClientApp (Element, Event (Maybe Photographer.Photographers))
mkContent = do
    (BTabs bTabs)        <- grab @BTabs

    (photographersContent, ePhotographers) <- mkPhotographersTab
    elseContent          <- liftUI $ UI.string "fuck2"
    elseContent2         <- liftUI $ UI.string "fuck2nodATA"

    let contents =
            fmap
                    (\xs ->
                        let focus = extract (Tab.unTabs xs)
                        in  case focus of
                                Tab.PhotographersTab -> [photographersContent]
                                _                    -> [elseContent]
                    )
                <$> bTabs

    content <- liftUI $ UI.div # sink children (fromMaybe [elseContent2] <$> contents)
    return (content, ePhotographers)




setup :: Window -> ClientApp ()
setup win = do
    liftUI $ return win # set title "test"


    _                               <- loginClient
    _                               <- getPhotographersClient
    _                               <- getTabsClient

    (elemTabs, eTabs)               <- mkTabs



    elemToken                       <- mkToken
    (elemContent, ePhotographers)                     <- mkContent



    (BPhotographers bPhotographers) <- grab @BPhotographers
    let childs =
            (   fmap toItem
            <$> (fromMaybe [])
            <$> (fmap (ListZipper.toList . Photographer.unPhotographers))
            <$> bPhotographers
            )
    elemPhotographers <- liftUI $ UI.div # sink items' childs

    liftUI
        $  getBody win
        #+ [ element elemToken
     --      , element elemPhotographers
           , UI.hr
           , element elemTabs
           , UI.hr
           , element elemContent
           ]

    onEvent' (filterJust eTabs) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostTabs postTabs) <- grab @PostTabs
        (HTabs    hTabs   ) <- grab @HTabs
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- postTabs (Token $ setCookieValue t) e
                liftIO $ hTabs (Just e)

    onEvent' (filterJust ePhotographers) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostPhotographers postPhotographers) <- grab @PostPhotographers
        (HPhotographers    hPhotographers ) <- grab @HPhotographers
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- postPhotographers (Token $ setCookieValue t) e
                liftIO $ hPhotographers (Just e)

    return ()


getTabsClient = do
    (GetTabs getTabs) <- grab @GetTabs
    (BToken  bToken ) <- grab @BToken
    (HTabs   hTabs  ) <- grab @HTabs
    token             <- currentValue bToken
    case token of
        Nothing -> liftIO $ die "Missing token"
        Just t  -> do
            res <- getTabs (Token $ setCookieValue t)
            case res of
                _ -> liftIO $ hTabs (Just res)

getPhotographersClient = do
    (GetPhotographers getPhotographers) <- grab @GetPhotographers
    (BToken           bToken          ) <- grab @BToken
    (HPhotographers   hPhotographers  ) <- grab @HPhotographers
    token                               <- currentValue bToken
    case token of
        Nothing -> liftIO $ die "Missing token"
        Just t  -> do
            res <- getPhotographers (Token $ setCookieValue t)
            case res of
                _ -> liftIO $ hPhotographers (Just res)

loginClient = do
    (Login login                       ) <- grab @Login
    (Headers NoContent (HCons res HNil)) <- login LoginForm
    (HToken hToken                     ) <- grab @HToken
    case res of
        MissingHeader ->
            liftIO
                $ die
                      "The server responded but the response was missing the right session header."
        UndecodableHeader _ ->
            liftIO
                $ die
                      "The server responded but the response had an undecodable session header."
        Header setCookieText -> do
            let cookies = parseSetCookie . encodeUtf8 <$> T.lines setCookieText
                jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
            case jwtCookie of
                Nothing ->
                    liftIO
                        $ die
                              "No JWT-Cookie was found in the Set-Cookie session header."
                Just setCookie -> liftIO $ hToken (Just setCookie)
