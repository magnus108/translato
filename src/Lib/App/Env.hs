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

data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan
    , port :: !Int
    , static :: !FilePath
    , index :: !FilePath
    }


newtype InChan = InChan { unInChan :: Chan.InChan Message.Message }
newtype OutChan = OutChan { unOutChan :: Chan.OutChan Message.Message }

instance Has InChan              (Env m) where
    obtain = inChan

instance Has OutChan              (Env m) where
    obtain = outChan


class Has field env where
    obtain :: env -> field

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
