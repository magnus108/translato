module Lib.App.Env where

import Lib.Utils

import           Servant.Auth.Server


data Env (m :: Type -> Type) = Env
    { clientPort :: !Int
    , serverPort :: !Int

    , static :: !FilePath
    , index :: !FilePath

    , mPhotographersFile :: MPhotographersFile
    , mTabsFile :: MTabsFile
    , mDumpFile :: MDumpFile

    , mDagsdatoFile :: MDagsdatoFile
    }


newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }

instance Has MPhotographersFile              (Env m) where
    obtain = mPhotographersFile

newtype MTabsFile = MTabsFile { unMTabsFile :: MVar FilePath }

instance Has MTabsFile              (Env m) where
    obtain = mTabsFile


newtype MDumpFile = MDumpFile { unMDumpFile :: MVar FilePath }

instance Has MDumpFile              (Env m) where
    obtain = mDumpFile

newtype MDagsdatoFile = MDagsdatoFile { unMDagsdatoFile :: MVar FilePath }

instance Has MDagsdatoFile              (Env m) where
    obtain = mDagsdatoFile
