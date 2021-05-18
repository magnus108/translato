module Lib.Api where

import           Data.UUID.Typed
import           Lib.Api.Types
import           Servant.API
import           Servant.API.Generic
import           Servant.Auth.Docs
import           Servant.HTML.Blaze


type SiteApi = ToApi Site

data Site route = Site
      { publicSite :: !(route :- PublicAPI)
      , protectedSite :: !(route :- ProtectedAPI)
      }
  deriving (Generic)


siteAPI :: Proxy SiteApi
siteAPI = genericApi $ (Proxy :: Proxy Site)


type PublicAPI = ToApi PublicSite

data PublicSite route
    = PublicSite { postLogin :: !(route :- PostLogin)
                 , getDocs :: !(route :- GetDocs)
                 }
  deriving (Generic)

publicSiteAPI :: Proxy PublicAPI
publicSiteAPI = genericApi $ (Proxy :: Proxy PublicSite)

type ProtectedAPI = ToApi ProtectedSite

data ProtectedSite route
  = ProtectedSite
      { protectedAccessKeySite :: !(route :- "access-key" :> ProtectedAccessKeyAPI)
      , photographers :: !(route :- "photographer" :> PhotographerAPI)
      , getPermissions :: !(route :- GetPermissions)
      }
  deriving (Generic)

-- PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
type PostLogin
    = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type GetDocs = Get '[HTML] GetDocsResponse
