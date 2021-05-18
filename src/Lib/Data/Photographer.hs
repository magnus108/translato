module Lib.Data.Photographer where

import           Lib.Data.Types
import           Data.Aeson
import           Utils.ListZipper
import           Servant.Docs

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

