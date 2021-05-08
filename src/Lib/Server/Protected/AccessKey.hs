module Lib.Server.Protected.AccessKey
  ( ProtectedAccessKeyAPI
  ,  ProtectedAccessKeySite (..)
  ,  AccessKeyUUID
  ,  AccessKeyInfo (..)
  ,  AddAccessKey (..)
  ,  AccessKeyCreated (..)
  ,  PostAddAccessKey
  ,  GetAccessKey
  , GetAccessKeys
  , DeleteAccessKey
  )
where

import           Lib.Server.Types               ( AppServer
                                                , ToApi
                                                , ProtectAPI
                                                , Permission
                                                )
import Data.UUID.Typed
import Servant.API
import Servant.API.Generic
import Servant.Auth.Docs ()
import Servant.Docs

import Lib.Server.Protected.AccessKey.Types

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
