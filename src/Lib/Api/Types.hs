module Lib.Api.Types where

import           Data.Time
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text.Encoding as TE

import qualified Data.UUID as UUID
import           Data.UUID.Typed
import           Servant.Docs
import           Servant.Auth.Server
import           Data.Aeson
import           Lib.Api.GetDocsResponse
import           Lib.Data.LoginForm
import           Lib.Data.Types
import           Servant.HTML.Blaze
import           Text.Blaze                    as HTML
import           Text.Blaze.Html               as HTML
import           Servant.API                   as Web
                                                ( (:>)
                                                , Capture
                                                , Headers
                                                , Get
                                                , Header
                                                , Header'
                                                , JSON
                                                , PostNoContent(..)
                                                , NoContent(NoContent)
                                                , Post
                                                , QueryParam
                                                , QueryParam'
                                                , ReqBody
                                                , (:<|>)
                                                )


import System.Random
import System.IO.Unsafe
import Lib.Data.Photographer
import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                , genericApi
                                                , ToServant
                                                )

import           Servant.API
import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                , genericApi
                                                , ToServantApi
                                                , ToServant
                                                )

type ToApi (site :: Type -> Type) = ToServantApi site

type RequiredHeader = Header' '[Required, Strict]


publicSiteAPI :: Proxy PublicAPI
publicSiteAPI = genericApi $ (Proxy :: Proxy PublicSite)

siteAPI :: Proxy SiteApi
siteAPI = genericApi $ (Proxy :: Proxy Site)


type SiteApi = ToApi Site

data Site route = Site
      { publicSite :: !(route :- PublicAPI)
      , protectedSite :: !(route :- ProtectedAPI)
      }
  deriving (Generic)

type ProtectedAPI = ToApi ProtectedSite

data PublicSite route
    = PublicSite { postLogin :: !(route :- PostLogin)
                 , getDocs :: !(route :- GetDocs)
                 }
  deriving (Generic)

type GetDocs = Get '[HTML] GetDocsResponse

type PublicAPI = ToApi PublicSite

data ProtectedSite route
  = ProtectedSite
      { protectedAccessKeySite :: !(route :- "access-key" :> ToApi ProtectedAccessKeySite)
      , photographers :: !(route :- "photographer" :> PhotographerAPI)
      , getPermissions :: !(route :- GetPermissions)
      }
  deriving (Generic)


type GetPermissions = ProtectAPI :> "permissions" :> Get '[JSON] [Permission]

 --- PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
type PostLogin
    = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

-------------------------------------------------------------------------------

data Permission
    = PermitAdd
    | WriteSomething
    | ReadSomthing
        deriving (Show, Read, Eq, Ord, Enum, Bounded)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)


instance ToSample Permission

type AccountUUID = UUID User

data User

type ProtectAPI = Auth '[JWT] AuthCookie

data AuthCookie
  = AuthCookie
      { userUUID :: AccountUUID,
        permissions :: [Permission]
      }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)
    deriving anyclass (FromJWT, ToJWT)

userPermissions :: [Permission]
userPermissions = [ReadSomthing]

adminOnlyPermissions :: [Permission]
adminOnlyPermissions = [WriteSomething]

adminPermissions :: [Permission]
adminPermissions = userPermissions ++ adminOnlyPermissions

allPermissions :: [Permission]
allPermissions = [minBound .. maxBound]

-------------------------------------------------------------------------------
type PhotographerAPI = ToApi PhotographerSite

data PhotographerSite route
  = PhotographerSite
      { getPhotographers :: !(route :- GetPhotographers)
      }
  deriving (Generic)


type GetPhotographers = ProtectAPI :> Get '[JSON] Photographers


-------------------------------------------------------------------------------
type ProtectedAccessKeyAPI = ToApi ProtectedAccessKeySite

data ProtectedAccessKeySite route
  = ProtectedAccessKeySite
      { postAddAccessKey :: !(route :- PostAddAccessKey)
      , getAccessKey :: !(route :- GetAccessKey)
      , getAccessKeys :: !(route :- GetAccessKeys)
      ,  deleteAccessKey :: !(route :- DeleteAccessKey)
      }
  deriving (Generic)

type PostAddAccessKey =
  ProtectAPI :> ReqBody '[JSON] AddAccessKey :> Post '[JSON] AccessKeyCreated

type GetAccessKey = ProtectAPI :> Capture "uuid" AccessKeyUUID :> Get '[JSON] AccessKeyInfo

type GetAccessKeys = ProtectAPI :> Get '[JSON] [AccessKeyInfo]

type DeleteAccessKey = ProtectAPI :> Capture "uuid" AccessKeyUUID :> Delete '[JSON] NoContent




instance ToCapture (Capture "uuid" AccessKeyUUID) where
  toCapture _ = DocCapture "uuid" "The UUID of the access key"

--------------------------------------------------------------------------------

type AccessKeyUUID = UUID AccessKey

data AccessKey

newtype AccessKeySecret
  = AccessKeySecret ByteString
    deriving (Show, Eq, Ord)
    deriving Generic


instance ToSample AccessKeySecret where
  toSamples Proxy = singleSample $ unsafePerformIO generateRandomAccessKeySecret

generateRandomAccessKeySecret :: IO AccessKeySecret
generateRandomAccessKeySecret = AccessKeySecret . SB.pack <$> replicateM 16 randomIO

instance FromJSON AccessKeySecret where
  parseJSON =
    withText "AccessKeySecret" $ \t ->
      case parseAccessKeySecretText t of
        Nothing -> fail "Invalid AccessKeySecret"
        Just aks -> pure aks

instance ToJSON AccessKeySecret where
  toJSON = toJSON . accessKeySecretText

accessKeySecretText :: AccessKeySecret -> Text
accessKeySecretText (AccessKeySecret bs) = TE.decodeUtf8 $ SB16.encode bs

parseAccessKeySecretText :: Text -> Maybe AccessKeySecret
parseAccessKeySecretText t =
  case SB16.decode $ TE.encodeUtf8 t of
    (d, "") -> Just $ AccessKeySecret d
    _ -> Nothing


data AccessKeyInfo
  = AccessKeyInfo
      { accessKeyInfoUUID :: AccessKeyUUID
      , accessKeyInfoName :: Text
      , accessKeyInfoCreatedTimestamp :: UTCTime
      , accessKeyInfoPermissions :: [Permission]
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)



instance ToSample AccessKeyInfo

data AddAccessKey
  = AddAccessKey
      { addAccessKeyName :: Text
      , addAccessKeyPermissions :: [Permission]
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

instance ToSample AddAccessKey

data AccessKeyCreated
  = AccessKeyCreated
      { accessKeyCreatedCreatedTimestamp :: UTCTime
      , accessKeyCreatedKey :: AccessKeySecret
      , accessKeyCreatedUUID :: AccessKeyUUID
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

instance ToSample AccessKeyCreated

instance ToSample UTCTime where
      toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample (UUID a) where
  toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)

--------------------------------------------------------------------------------
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


