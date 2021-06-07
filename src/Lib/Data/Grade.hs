module Lib.Data.Grade where

import           Data.Aeson
import           Utils.ListZipper
import           Lib.Data.Permission
import qualified Lib.Data.Photographee as Photographee

type Identifier = Text

data Grade = Grade
    { identifier :: Identifier
    , photographees :: Photographee.Photographees
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Grades = Grades { unGrades :: ListZipper Grade }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

sempty :: Grade
sempty = Grade "" Photographee.semptys
