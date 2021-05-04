module Lib.Server.Auth
    ( LoginRequest(..)
    , LoginResponse(..)
    , AuthApi
    , AuthSite
    , authServer
--    , loginHandler
--    , isLoggedInHandler
--    , logoutHandler
    )
where


import           Lib.App.Error                  ( WithError
                                                , throwError
                                                , throwOnNothingM
                                                )

import           Lib.Server.Types               ( AppServer
                                                , ToApi
                                                )


import           Data.Aeson
import           Servant.API                   as Web
                                                ( (:>)
                                                , Capture
                                                , Get
                                                , Header
                                                , Header'
                                                , JSON
                                                , NoContent(NoContent)
                                                , Post
                                                , QueryParam
                                                , QueryParam'
                                                , ReqBody
                                                )
import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                )
import           Servant.Docs

data LoginRequest = LoginRequest
    { --loginRequestEmail    :: Email
    -- , loginRequestPassword :: PasswordPlainText
    } deriving Show
      deriving Generic
      deriving anyclass (FromJSON, ToJSON)
      deriving ToSample

data LoginResponse = LoginResponse
    { -- loginResponseToken :: JwtToken
    } deriving Show
      deriving Generic
      deriving anyclass (FromJSON, ToJSON)
      deriving ToSample


data AuthSite route = AuthSite
    { loginRoute :: route
        :- "login"
        :> ReqBody '[JSON] LoginRequest
        :> Post '[JSON] LoginResponse

    , isLoggedInRoute :: route
        :- "login"
        :> Capture "JWT" String -- JwtToken
        :> Get '[JSON] NoContent

    , logoutRoute :: route
        :- "logout"
        :> Capture "JWT" String -- JwtToken
        :> Get '[JSON] NoContent
    } deriving (Generic)


instance ToCapture (Capture "JWT" String) where
  toCapture _ = DocCapture "JWT" "No description"


type AuthApi = ToApi AuthSite


authServer :: AuthSite AppServer
authServer = AuthSite { loginRoute      = undefined -- loginHandler
                      , isLoggedInRoute = undefined -- isLoggedInHandler
                      , logoutRoute     = undefined -- logoutHandler
                      }
