{-# LANGUAGE GADTs #-}
module Lib.Client where

import           Servant.Auth.Client
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
import           Lib.Api                 hiding ( GetPhotographers )


setup :: Window -> ClientApp ()
setup win = do
    liftUI $ return win # set title "test"


    _               <- loginClient
    _               <- getPhotographersClient


    (BToken bToken) <- grab @BToken
    elem            <- liftUI $ UI.p # sink
        text
        (show <$> (fromMaybe (B.empty)) <$> (fmap setCookieValue) <$> bToken)
    liftUI $ getBody win #+ [element elem]
    return ()

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
