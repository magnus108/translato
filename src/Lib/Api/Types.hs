module Lib.Api.Types where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import qualified Data.UUID                     as UUID
import           Data.UUID.Typed
import           Servant.API
import           Servant.Auth
import           Servant.Auth.Docs
import           Servant.Auth.Server
import           Servant.Docs
import           Servant.HTML.Blaze
import           System.IO.Unsafe
import           Text.Blaze                    as HTML
import           Text.Blaze.Html               as HTML
import           Lib.Data.Permission
import           Lib.Data.Username
import           Lib.Data.AccountUUID
import           Servant.API.Generic            ( ToServantApi )

type ToApi (site :: Type -> Type) = ToServantApi site

type RequiredHeader = Header' '[Required, Strict]

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


newtype GetDocsResponse
  = GetDocsResponse
      { unGetDocsResponse :: HTML.Html
      }
        deriving Generic

instance MimeUnrender HTML GetDocsResponse where
    mimeUnrender Proxy bs =
        Right $ GetDocsResponse $ HTML.unsafeLazyByteString bs

instance ToSample GetDocsResponse where
    toSamples Proxy = singleSample $ GetDocsResponse "Documentation (In HTML)."

instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html

data LoginForm
  = LoginForm
      { loginFormUsername :: Username
      , loginFormPassword :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

instance ToSample LoginForm


instance ToSample UTCTime where
    toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample (UUID a) where
    toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)


type GetPermissions = ProtectAPI :> "permissions" :> Get '[JSON] [Permission]
