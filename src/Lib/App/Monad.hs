module Lib.App.Monad
    ( App(..)
    , AppEnv
    , runAppAsIO
    , runApp
    )
where

import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Relude.Extra                   ( firstF )

import           Lib.App.Env                    ( Env )
import           Lib.App.Error                  ( AppError
                                                , AppException(..)
                                                )


type AppEnv = Env App


newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving newtype ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader AppEnv
               )

instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \e -> runApp env $ handler $ unAppException e


runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env


runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp
