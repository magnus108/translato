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

type AppServer = AsServerT ServerApp

type ServerAppEnv = ServerEnv ServerApp

data ServerEnv (m :: Type -> Type) = ServerEnv
    { mPhotographersFile :: MPhotographersFile
    , mTabsFile :: MTabsFile
    , cookieSettings :: !CookieSettings
    , jwtSettings :: !JWTSettings
    }

newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }
newtype MTabsFile = MTabsFile { unMTabsFile :: MVar FilePath }

instance Has MPhotographersFile              (ServerEnv m) where
    obtain = mPhotographersFile

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
