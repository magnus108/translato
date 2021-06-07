module Lib.Data.Location where


import           Data.Aeson

newtype Location = Location { unLocation :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
