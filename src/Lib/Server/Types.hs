module Lib.Server.Types where

import           Servant.Auth.Server
import           Servant.Server.Generic         ( AsServerT )
import           Control.Monad.Except           ( MonadError(..) )

import           Lib.Server.Error
import           Relude.Extra                   ( firstF )
import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )

import           Lib.Utils
import Control.Monad.Trans.Resource

type AppServer = AsServerT ServerApp

type ServerAppEnv = ServerEnv ServerApp

data ServerEnv (m :: Type -> Type) = ServerEnv
    { mPhotographersFile :: MPhotographersFile
    , mTabsFile :: MTabsFile
    , mCamerasFile :: MCamerasFile
    , mDumpFile :: MDumpFile
    , mDagsdatoFile :: MDagsdatoFile
    , mDoneshootingFile :: MDoneshootingFile
    , mDagsdatoBackupFile :: MDagsdatoBackupFile
    , mShootingsFile :: MShootingsFile
    , mSessionsFile :: MSessionsFile
    , mLocationFile :: MLocationFile
    , mGradesFile :: MGradesFile
    , cookieSettings :: !CookieSettings
    , jwtSettings :: !JWTSettings
    }

instance FilePathable MGradesFile where
    toFilePath (MGradesFile mFile) = mFile


instance FilePathable MLocationFile where
    toFilePath (MLocationFile mFile) = mFile

instance FilePathable MShootingsFile where
    toFilePath (MShootingsFile mFile) = mFile

instance FilePathable MCamerasFile where
    toFilePath (MCamerasFile mFile) = mFile

instance FilePathable MSessionsFile where
    toFilePath (MSessionsFile mFile) = mFile

newtype MLocationFile = MLocationFile { unMLocationFile :: MVar FilePath }
newtype MSessionsFile = MSessionsFile { unMSessionsFile :: MVar FilePath }
newtype MShootingsFile = MShootingsFile { unMShootingsFile :: MVar FilePath }
newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }
newtype MTabsFile = MTabsFile { unMTabsFile :: MVar FilePath }
newtype MDumpFile = MDumpFile { unMDumpFile :: MVar FilePath }
newtype MCamerasFile = MCamerasFile { unMCamerasFile :: MVar FilePath }
newtype MDagsdatoFile = MDagsdatoFile { unMDagsdatoFile :: MVar FilePath }
newtype MDoneshootingFile = MDoneshootingFile { unMDoneshootingFile :: MVar FilePath }
newtype MDagsdatoBackupFile = MDagsdatoBackupFile { unMDagsdatoBackupFile :: MVar FilePath }
newtype MGradesFile = MGradesFile { unMGradesFile :: MVar FilePath }

instance Has MLocationFile              (ServerEnv m) where
    obtain = mLocationFile

instance Has MShootingsFile              (ServerEnv m) where
    obtain = mShootingsFile

instance Has MSessionsFile              (ServerEnv m) where
    obtain = mSessionsFile

instance Has MPhotographersFile              (ServerEnv m) where
    obtain = mPhotographersFile

instance Has MCamerasFile              (ServerEnv m) where
    obtain = mCamerasFile


instance Has MDumpFile              (ServerEnv m) where
    obtain = mDumpFile

instance Has MDagsdatoFile              (ServerEnv m) where
    obtain = mDagsdatoFile

instance Has MDoneshootingFile              (ServerEnv m) where
    obtain = mDoneshootingFile

instance Has MDagsdatoBackupFile              (ServerEnv m) where
    obtain = mDagsdatoBackupFile

instance Has MGradesFile              (ServerEnv m) where
    obtain = mGradesFile



instance Has MTabsFile              (ServerEnv m) where
    obtain = mTabsFile

instance Has CookieSettings              (ServerEnv m) where
    obtain = cookieSettings

instance Has JWTSettings              (ServerEnv m) where
    obtain = jwtSettings


newtype ServerApp a = ServerApp
    { unServerApp :: ReaderT ServerAppEnv IO a
    } deriving newtype ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadUnliftIO
               , MonadReader ServerAppEnv
               )

instance MonadError ServerAppError ServerApp where
    throwError :: ServerAppError -> ServerApp a
    throwError = liftIO . throwIO . ServerAppException

    catchError :: ServerApp a -> (ServerAppError -> ServerApp a) -> ServerApp a
    catchError action handler = ServerApp $ ReaderT $ \env -> do
        let ioAction = runServerApp env action
        ioAction `catch` \e ->
            runServerApp env $ handler $ unServerAppException e


runServerAppAsIO :: ServerAppEnv -> ServerApp a -> IO (Either ServerAppError a)
runServerAppAsIO env = firstF unServerAppException . try . runServerApp env


runServerApp :: ServerAppEnv -> ServerApp a -> IO a
runServerApp env = usingReaderT env . unServerApp
