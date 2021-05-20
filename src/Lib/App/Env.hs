{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env where

import Lib.Utils
import           Lib.Api
import           Lib.Api.Types
import           Lib.Data.Photographer          ( Photographers )

import           Servant.API
import           Servant.Auth.Client
import           Servant.Client
import           Servant.Client




import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Relude.Extra                   ( firstF )

import           Lib.App.Error                  ( AppError
                                                , AppException(..)
                                                )



import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Message                   as Message

import           Servant.Client
import           Servant.Auth.Server


-------------------------------------------------------------------------------
import qualified Data.UUID                     as UUID
import           Data.Aeson
import           Data.Set                       ( Set )
import           Data.Time
import           Data.UUID.Typed

import qualified Data.ByteString               as SB
import qualified Data.ByteString.Base16        as SB16
import qualified Data.Text.Encoding            as TE

import           System.Random
import           System.IO.Unsafe
import           Utils.ListZipper
import qualified Data.UUID                     as UUID
import           Servant.Docs
import           Data.Aeson
import           Data.Set                       ( Set )
import           Data.Time
import           Data.UUID.Typed
import           Data.Aeson
import           Servant.Auth.Docs


import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           Servant.Server
import           Servant.Auth.Server
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )

import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                , genericApi
                                                , ToServantApi
                                                , ToServant
                                                )




-------------------------------------------------------------------------------


data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan
    , clientPort :: !Int
    , serverPort :: !Int
    , static :: !FilePath
    , index :: !FilePath

    , mPhotographersFile :: MPhotographersFile
    , cookieSettings :: !CookieSettings
    , jwtSettings :: !JWTSettings
    }


newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }

newtype InChan = InChan { unInChan :: Chan.InChan Message.Message }
newtype OutChan = OutChan { unOutChan :: Chan.OutChan Message.Message }

instance Has MPhotographersFile              (Env m) where
    obtain = mPhotographersFile

instance Has CookieSettings              (Env m) where
    obtain = cookieSettings

instance Has JWTSettings              (Env m) where
    obtain = jwtSettings


instance Has OutChan              (Env m) where
    obtain = outChan


--------------------------------------------------------------------------------
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
