{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
where

import           Servant.API
import           Servant.Auth.Client
import           Servant.Client
import           Servant.Client
import           Servant.Auth.Server




import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Relude.Extra                   ( firstF )

import           Lib.App.Error                  ( AppError
                                                , AppException(..)
                                                )



import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Message                   as Message

import           Servant.Client
import           Servant.Auth.Server


-------------------------------------------------------------------------------
import Lib.Server.Protected.Types
import qualified Data.UUID as UUID
import Servant.Docs
import           Data.Aeson
import           Data.Set                       ( Set )
import           Data.Time
import           Data.UUID.Typed

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text.Encoding as TE


import System.Random
import System.IO.Unsafe
import Utils.ListZipper
import qualified Data.UUID as UUID
import Servant.Docs
import           Data.Aeson
import           Data.Set                       ( Set )
import           Data.Time
import           Data.UUID.Typed
import           Data.Aeson
import           Servant.Auth.Docs

import qualified Servant.Docs                  as Docs

import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import Servant.Server
import           Servant.Auth.Server
import           Servant.Server.Generic         ( AsServerT, genericServerT )

import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                , genericApi
                                                , ToServantApi
                                                , ToServant
                                                )


import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as LT
import qualified Text.Markdown                 as Markdown

import           Servant.HTML.Blaze
import           Text.Blaze                    as HTML
import           Text.Blaze.Html               as HTML

-------------------------------------------------------------------------------


data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan
    , clientPort :: !Int
    , serverPort :: !Int
    , static :: !FilePath
    , index :: !FilePath

    , clients :: !Clients

    , mPhotographersFile :: MPhotographersFile

    , cookieSettings :: !CookieSettings
    , jwtSettings :: !JWTSettings
    }



newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }
newtype InChan = InChan { unInChan :: Chan.InChan Message.Message }
newtype OutChan = OutChan { unOutChan :: Chan.OutChan Message.Message }


instance Has MPhotographersFile              (Env m) where
    obtain = mPhotographersFile

instance Has CookieSettings              (Env m) where
    obtain = cookieSettings

instance Has JWTSettings              (Env m) where
    obtain = jwtSettings

instance Has OutChan              (Env m) where
    obtain = outChan


class Has field env where
    obtain :: env -> field

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field


--------------------------------------------------------------------------------
type AppEnv = Env App


newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving newtype ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader AppEnv
               )

instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \e -> runApp env $ handler $ unAppException e


runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env


runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp



--------------------------------------------------------------------------------
data Clients = Clients
    { login :: LoginForm -> App (Headers '[Header "Set-Cookie" Text] NoContent)
    , getPhotographers' :: Token -> App Photographers
    }

--------------------------------------------------------------------------------
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

type PostLogin
    = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
   --- PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
   --

newtype Username
  = Username
      { usernameText :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

data LoginForm
  = LoginForm
      { loginFormUsername :: Username
      , loginFormPassword :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

instance Docs.ToSample LoginForm
instance Docs.ToSample Username


newtype GetDocsResponse
  = GetDocsResponse
      { unGetDocsResponse :: HTML.Html
      }
        deriving Generic

instance Docs.ToSample GetDocsResponse where
    toSamples Proxy =
        Docs.singleSample $ GetDocsResponse "Documentation (In HTML)."
instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html
instance MimeUnrender HTML GetDocsResponse where
    mimeUnrender Proxy bs =
        Right $ GetDocsResponse $ HTML.unsafeLazyByteString bs


htmlResponse :: GetDocsResponse
htmlResponse =
    GetDocsResponse
        $ Markdown.markdown Markdown.defaultMarkdownSettings
              { Markdown.msXssProtect = False
              }
        $ LT.fromStrict
        $ docs2

docs2 :: Text
docs2 =
    T.unlines
        . map
              (\t -> if T.isPrefixOf "```" (T.stripStart t)
                  then T.stripStart t
                  else t
              )
        . T.lines
        . T.pack
        $ Docs.markdown
        $ docs'




docs' :: Docs.API
docs' = Docs.docs publicSiteAPI

-------------------------------------------------------------------------------

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

type RequiredHeader = Header' '[Required, Strict]



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


type Name = Text
type Tid = Text

data Photographer = Photographer
    { name :: Name
    , tid :: Tid
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


instance (ToSample a) => ToSample (ListZipper a)

instance ToSample Photographer
instance ToSample Photographers

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


