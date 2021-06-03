module Lib.Data.Camera where


import           Data.Aeson
import           Utils.ListZipper


data Camera
    = CR2
    | CR3
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Cameras = Cameras { unCameras :: ListZipper Camera }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
