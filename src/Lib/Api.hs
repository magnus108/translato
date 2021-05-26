module Lib.Api where

import           Lib.Api.Types
import           Servant.API
import           Servant.API.Generic
import           Servant.HTML.Blaze
import           Lib.Data.Photographer
import           Lib.Data.Tab


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
      { photographers :: !(route :- "photographer" :> PhotographerAPI)
      , tabs :: !(route :- "tab" :> TabAPI)
      , getPermissions :: !(route :- GetPermissions)
      }
  deriving (Generic)

-- PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
type PostLogin
    = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type GetDocs = Get '[HTML] GetDocsResponse


type PhotographerAPI = ToApi PhotographerSite

data PhotographerSite route
  = PhotographerSite
      { getPhotographers :: !(route :- GetPhotographers)
      }
  deriving (Generic)


type GetPhotographers = ProtectAPI :> Get '[JSON] Photographers


type TabAPI = ToApi TabSite

data TabSite route
  = TabSite
      { getTabs :: !(route :- GetTabs)
      }
  deriving (Generic)


type GetTabs = ProtectAPI :> Get '[JSON] Tabs

