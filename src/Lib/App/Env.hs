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

    , mCamerasFile :: MCamerasFile

    , mDumpFile :: MDumpFile

    , mDagsdatoFile :: MDagsdatoFile
    , mDagsdatoBackupFile :: MDagsdatoBackupFile
    
    , mDoneshootingFile :: MDoneshootingFile 
    }


newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }

instance Has MPhotographersFile              (Env m) where
    obtain = mPhotographersFile

newtype MTabsFile = MTabsFile { unMTabsFile :: MVar FilePath }

instance Has MTabsFile              (Env m) where
    obtain = mTabsFile

newtype MCamerasFile = MCamerasFile { unMCamerasFile :: MVar FilePath }

instance Has MCamerasFile              (Env m) where
    obtain = mCamerasFile


newtype MDumpFile = MDumpFile { unMDumpFile :: MVar FilePath }

instance Has MDumpFile              (Env m) where
    obtain = mDumpFile

newtype MDagsdatoFile = MDagsdatoFile { unMDagsdatoFile :: MVar FilePath }

instance Has MDagsdatoFile              (Env m) where
    obtain = mDagsdatoFile

newtype MDoneshootingFile = MDoneshootingFile { unMDoneshootingFile :: MVar FilePath }

instance Has MDoneshootingFile              (Env m) where
    obtain = mDoneshootingFile

newtype MDagsdatoBackupFile = MDagsdatoBackupFile { unMDagsdatoBackupFile :: MVar FilePath }

instance Has MDagsdatoBackupFile              (Env m) where
    obtain = mDagsdatoBackupFile
