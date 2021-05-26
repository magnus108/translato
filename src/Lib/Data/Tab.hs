module Lib.Data.Tab where

import           Data.Aeson
import           Utils.ListZipper


data Tab
    = DumpTab
    | DagsdatoTab
    | DagsdatoBackupTab
    | DoneshootingTab
    | DoneshootingBackupTab
    | PhotographersTab
    | CamerasTab
    | ShootingsTab
    | SessionsTab
    | LocationTab
    | MainTab
    | InsertPhotographeeTab
    | ControlTab
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Tabs = Tabs { unTabs :: ListZipper Tab }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
