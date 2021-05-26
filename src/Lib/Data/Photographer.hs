module Lib.Data.Photographer where

import           Data.Aeson
import           Utils.ListZipper
import           Lib.Data.Permission

type Name = Text
type Tid = Text

data Photographer = Photographer
    { name :: Name
    , tid :: Tid
    , perms :: [Permission]
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
