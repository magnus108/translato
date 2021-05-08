module Lib.Server.Types where

import           Servant.API                    ( Header'
                                                , Required
                                                , Strict
                                                )
import           Data.Aeson
import           Servant.API.Generic            ( ToServantApi )
import           Servant.Server.Generic         ( AsServerT )

import           Lib.App                        ( App )

import           Servant.Auth
import           Servant.Auth.Docs              ( )
import           Servant.Auth.Server

import qualified Data.Set                      as S
import           Data.Set                       ( Set )

import Data.UUID.Typed

import Servant.Docs

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
