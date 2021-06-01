module Lib.Api where

import           Lib.Api.Types
import           Servant.API
import           Servant.API.Generic
import           Servant.HTML.Blaze
import           Lib.Data.Photographer
import           Lib.Data.Dump
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
      , tabSite :: !(route :- "tab" :> TabAPI)
      , dumpSite :: !(route :- "dump" :> DumpAPI)
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
      , postPhotographers :: !(route :- PostPhotographers)
      }
  deriving (Generic)


type GetPhotographers = ProtectAPI :> Get '[JSON] Photographers
type PostPhotographers = ProtectAPI :> ReqBody '[JSON] Photographers :> Post '[JSON] NoContent


type TabAPI = ToApi TabSite

data TabSite route
  = TabSite
      { getTabs :: !(route :- GetTabs)
      , postTabs :: !(route :- PostTabs)
      }
  deriving (Generic)


type PostTabs = ProtectAPI :> ReqBody '[JSON] Tabs :> Post '[JSON] NoContent

type GetTabs = ProtectAPI :> Get '[JSON] Tabs


type DumpAPI = ToApi DumpSite

data DumpSite route
  = DumpSite
      { getDump :: !(route :- GetDump)
      , postDump :: !(route :- PostDump)
      }
  deriving (Generic)


type PostDump = ProtectAPI :> ReqBody '[JSON] Dump :> Post '[JSON] NoContent

type GetDump = ProtectAPI :> Get '[JSON] Dump

