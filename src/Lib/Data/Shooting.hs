module Lib.Data.Shooting where


import           Data.Aeson
import           Utils.ListZipper


data Shooting
    = ReShoot
    | Normal
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Shootings = Shootings { unShootings :: ListZipper Shooting }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
