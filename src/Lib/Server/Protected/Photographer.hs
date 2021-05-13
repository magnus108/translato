module Lib.Server.Protected.Photographer 
    (PhotographerSite(..), PhotographerAPI, Photographer(..), Photographers(..))
    where

import Lib.Server.Protected.Types
import           Lib.Server.Types               ( AppServer
                                                , ToApi
                                                , ProtectAPI
                                                , Permission
                                                )
import           Utils.ListZipper
import           Data.Aeson
import           Data.UUID.Typed
import           Servant.API
import           Servant.API.Generic
import           Servant.Auth.Docs              ( )
import           Servant.Docs


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
