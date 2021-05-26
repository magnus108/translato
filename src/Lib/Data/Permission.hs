module Lib.Data.Permission where

import           Data.Aeson
import           Servant.Docs

data Permission
    = Full
    | Simple
        deriving (Show, Read, Eq, Ord, Enum, Bounded)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

instance ToSample Permission

userPermissions :: [Permission]
userPermissions = [Simple]

adminOnlyPermissions :: [Permission]
adminOnlyPermissions = [Full]

adminPermissions :: [Permission]
adminPermissions = userPermissions ++ adminOnlyPermissions

allPermissions :: [Permission]
allPermissions = [minBound .. maxBound]
