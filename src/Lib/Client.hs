{-# LANGUAGE GADTs #-}
module Lib.Client where

import           Servant.Auth.Client
import           Control.Conditional            ( (?<>) )
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Lib.Client.Types
import           Lib.Utils
import           Lib.Api.Types
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
                                                , GetTabs
                                                )
import qualified Utils.ListZipper              as ListZipper
import qualified Control.Lens                  as Lens
import qualified Lib.Data.Photographer         as Photographer
import qualified Lib.Data.Tab                  as Tab
import           Control.Comonad


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
        <$> (fmap (ListZipper.toList . ListZipper.bextend display . Tab.unTabs))
        <$> bTabs
        )

    return (item, eSelection)

mkToken = do
    (BToken bToken) <- grab @BToken
    liftUI $ UI.p # sink
        text
        (show <$> (fromMaybe (B.empty)) <$> (fmap setCookieValue) <$> bToken)


mkContent :: ClientApp Element
mkContent = do
    (BTabs bTabs)        <- grab @BTabs

    photographersContent <- liftUI $ UI.string "fuck"
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

    liftUI $ UI.div # sink children (fromMaybe [elseContent2] <$> contents)




setup :: Window -> ClientApp ()
setup win = do
    liftUI $ return win # set title "test"


    _                               <- loginClient
    _                               <- getPhotographersClient
    _                               <- getTabsClient

    (elemTabs, eTabs)               <- mkTabs
    --- dont do this
    (HTabs hTabs)            <- grab @HTabs
    _ <- liftIO $ register eTabs hTabs



    elemToken                       <- mkToken
    elemContent                     <- mkContent



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
    return ()

toItem :: Show a => a -> UI Element
toItem a = UI.string (show a)

items :: Show a => ReadWriteAttr Element [a] ()
items = mkWriteAttr $ \is x -> void $ do
    return x # set children [] #+ fmap (\i -> UI.string (show i)) is

items' :: ReadWriteAttr Element [UI Element] ()
items' = mkWriteAttr $ \is x -> void $ do
    is' <- sequence is
    return x # set children is'


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
