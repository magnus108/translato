{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
    ( Env(..)
    , Has(..)
    , InChan(..)
    , OutChan(..)
    , grab
    )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Message                   as Message

import           Servant.Client
import           Servant.Auth.Server

data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan
    , clientPort :: !Int
    , serverPort :: !Int
    , static :: !FilePath
    , index :: !FilePath
    , cenv :: !ClientEnv

    , cookieSettings :: !CookieSettings
    , jwtSettings :: !JWTSettings
    }


newtype InChan = InChan { unInChan :: Chan.InChan Message.Message }
newtype OutChan = OutChan { unOutChan :: Chan.OutChan Message.Message }


instance Has CookieSettings              (Env m) where
    obtain = cookieSettings

instance Has JWTSettings              (Env m) where
    obtain = jwtSettings

instance Has OutChan              (Env m) where
    obtain = outChan


class Has field env where
    obtain :: env -> field

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
