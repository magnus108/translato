module Lib.Data.Permission where

import           Data.Aeson
import           Servant.Docs

data Permission
    = PermitAdd
    | WriteSomething
    | ReadSomthing
        deriving (Show, Read, Eq, Ord, Enum, Bounded)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

instance ToSample Permission

userPermissions :: [Permission]
userPermissions = [ReadSomthing]

adminOnlyPermissions :: [Permission]
adminOnlyPermissions = [WriteSomething]

adminPermissions :: [Permission]
adminPermissions = userPermissions ++ adminOnlyPermissions

allPermissions :: [Permission]
allPermissions = [minBound .. maxBound]
