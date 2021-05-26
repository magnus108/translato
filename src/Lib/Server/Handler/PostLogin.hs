module Lib.Server.Handler.PostLogin where

import           Lib.Data.Permission
import           Lib.Api.Types
import           Lib.Server.Types

import           Blaze.ByteString.Builder       ( toByteString )

import           Data.UUID.Typed
import           Servant                 hiding ( BadPassword
                                                , NoSuchUser
                                                , throwError
                                                , ServerError
                                                )
import           Lib.Server.Error
import           Servant.Auth.Server           as Auth
import           Web.Cookie

servePostLogin
    :: LoginForm -> ServerApp (Headers '[Header "Set-Cookie" Text] NoContent)
servePostLogin LoginForm = do
    let perms = adminPermissions
        uid   = 1
            --Find ud af hvordan User virker??
    userIdentifier <- liftIO nextRandomUUID
    traceShowM "lol"
    setLoggedIn uid userIdentifier perms
  where
    setLoggedIn uid userIdentifier perms = do
        let cookie =
                AuthCookie { userUUID = userIdentifier, permissions = perms }
        ServerEnv {..} <- ask -- this is wrong
        mCookie <- liftIO $ makeSessionCookie cookieSettings jwtSettings cookie
        case mCookie of
            Nothing        -> throwError $ ServerError "servantErr"
            Just setCookie -> do
                --now <- liftIO getCurrentTime
                --runDb $ update uid [UserLastLogin =. Just now]
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
