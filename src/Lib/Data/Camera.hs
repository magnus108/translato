module Lib.Data.Camera where


import           Data.Aeson

newtype Camera = Camera { unCamera :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
