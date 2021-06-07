module Lib.Data.Photographee
    ( Photographee(..)
    , Photographees(..)
    , sempty
    , semptys
    )
where

import           Data.Aeson
import           Utils.ListZipper
import           Lib.Data.Permission

type Identifier = Text
type Sys = Text
type Name = Text

data Photographee = Photographee
    { name :: Name
    , identifier :: Identifier
    , sys :: Sys
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Photographees = Photographees { unPhotographees :: ListZipper Photographee }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


sempty :: Photographee
sempty = Photographee "" "" ""

semptys :: Photographees
semptys = Photographees $ ListZipper [] sempty []
