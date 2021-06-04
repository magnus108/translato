module Lib.Api where

import           Lib.Api.Types
import           Servant.API
import           Servant.API.Generic
import           Servant.HTML.Blaze
import           Lib.Data.Photographer
import           Lib.Data.Dump
import           Lib.Data.Camera
import           Lib.Data.Dagsdato
import           Lib.Data.Doneshooting
import           Lib.Data.DagsdatoBackup
import           Lib.Data.Tab
import           Lib.Data.Shooting


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
      , dagsdatoSite :: !(route :- "dagsdato" :> DagsdatoAPI)
      , dagsdatoBackupSite :: !(route :- "dagsdatoBackup" :> DagsdatoBackupAPI)
      , doneshootingSite :: !(route :- "doneshooting" :> DoneshootingAPI)
      , cameraSite :: !(route :- "camera" :> CameraAPI)
      , shootingSite :: !(route :- "shooting" :> ShootingAPI)
      --, getPermissions :: !(route :- GetPermissions)
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
type PostPhotographers
    = ProtectAPI :> ReqBody '[JSON] Photographers :> Post '[JSON] NoContent


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





type DagsdatoAPI = ToApi DagsdatoSite

data DagsdatoSite route
  = DagsdatoSite
      { getDagsdato :: !(route :- GetDagsdato)
      , postDagsdato :: !(route :- PostDagsdato)
      }
  deriving (Generic)


type PostDagsdato
    = ProtectAPI :> ReqBody '[JSON] Dagsdato :> Post '[JSON] NoContent

type GetDagsdato = ProtectAPI :> Get '[JSON] Dagsdato


type DagsdatoBackupAPI = ToApi DagsdatoBackupSite

data DagsdatoBackupSite route
  = DagsdatoBackupSite
      { getDagsdatoBackup :: !(route :- GetDagsdatoBackup)
      , postDagsdatoBackup :: !(route :- PostDagsdatoBackup)
      }
  deriving (Generic)


type PostDagsdatoBackup
    = ProtectAPI :> ReqBody '[JSON] DagsdatoBackup :> Post '[JSON] NoContent

type GetDagsdatoBackup = ProtectAPI :> Get '[JSON] DagsdatoBackup


type DoneshootingAPI = ToApi DoneshootingSite

data DoneshootingSite route
  = DoneshootingSite
      { getDoneshooting :: !(route :- GetDoneshooting)
      , postDoneshooting :: !(route :- PostDoneshooting)
      }
  deriving (Generic)


type PostDoneshooting
    = ProtectAPI :> ReqBody '[JSON] Doneshooting :> Post '[JSON] NoContent

type GetDoneshooting = ProtectAPI :> Get '[JSON] Doneshooting


type CameraAPI = ToApi CameraSite

data CameraSite route
  = CameraSite
      { getCameras :: !(route :- GetCameras )
      , postCameras :: !(route :- PostCameras )
      }
  deriving (Generic)


type PostCameras
    = ProtectAPI :> ReqBody '[JSON] Cameras :> Post '[JSON] NoContent

type GetCameras = ProtectAPI :> Get '[JSON] Cameras


type ShootingAPI = ToApi ShootingSite

data ShootingSite route
  = ShootingSite
      { getShootings :: !(route :- GetShootings )
      , postShootings :: !(route :- PostShootings )
      }
  deriving (Generic)


type PostShootings
    = ProtectAPI :> ReqBody '[JSON] Shootings :> Post '[JSON] NoContent

type GetShootings = ProtectAPI :> Get '[JSON] Shootings
