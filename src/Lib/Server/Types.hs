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



type AccountUUID = UUID User

data User

type ProtectAPI = Auth '[JWT] AuthCookie

data AuthCookie
  = AuthCookie
      { userUUID :: AccountUUID,
        permissions :: Set Permission
      }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)
    deriving anyclass (FromJWT, ToJWT)

userPermissions :: Set Permission
userPermissions = S.fromList [ReadSomthing]

adminOnlyPermissions :: Set Permission
adminOnlyPermissions = S.fromList [WriteSomething]

adminPermissions :: Set Permission
adminPermissions = S.union userPermissions adminOnlyPermissions

allPermissions :: Set Permission
allPermissions = S.fromList [minBound .. maxBound]
