module Lib.App.Env where

import Lib.Utils

import           Servant.Auth.Server


data Env (m :: Type -> Type) = Env
    { clientPort :: !Int
    , serverPort :: !Int

    , static :: !FilePath
    , index :: !FilePath

    , mPhotographersFile :: MPhotographersFile
    }


newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }

instance Has MPhotographersFile              (Env m) where
    obtain = mPhotographersFile

