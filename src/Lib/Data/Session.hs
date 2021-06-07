module Lib.Data.Session where


import           Data.Aeson
import           Utils.ListZipper


data Session
    = KindergartenGroup
    | KindergartenSingle
    | School
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype Sessions = Sessions { unSessions :: ListZipper Session }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
