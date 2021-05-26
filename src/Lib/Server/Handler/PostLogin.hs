module Lib.Server.Handler.PostLogin where

import           Lib.Data.Permission
import           Lib.Api.Types
import           Lib.Server.Types

import           Blaze.ByteString.Builder       ( toByteString )

import           Servant                 hiding ( BadPassword
                                                , NoSuchUser
                                                , throwError
                                                , ServerError
                                                )
import           Lib.Server.Error
import           Servant.Auth.Server           as Auth
import           Web.Cookie
import           Control.Lens                   ( (^.) )
import           Lib.Server.Handler.GetPhotographers
import           Lib.Data.Photographer
import           Control.Comonad
import Data.Generics.Labels ()

servePostLogin
    :: LoginForm -> ServerApp (Headers '[Header "Set-Cookie" Text] NoContent)
servePostLogin LoginForm = do
    (Photographers photographers) <- readPhotographers
    let perms = (extract photographers) ^. #perms
    setLoggedIn perms
  where
    setLoggedIn perms = do
        let cookie = AuthCookie { permissions = perms }
        ServerEnv {..} <- ask
        mCookie <- liftIO $ makeSessionCookie cookieSettings jwtSettings cookie
        case mCookie of
            Nothing        -> throwError $ ServerError "servantErr"
            Just setCookie -> do
                return $ addHeader
                    (decodeUtf8
                        ((toByteString . renderSetCookie)
                            (setCookie { setCookieSecure   = False
                                       , setCookieHttpOnly = False
                                       }
                            )
                        )
                    )
                    NoContent
