module Lib.Data.Doneshooting where


import           Data.Aeson

newtype Doneshooting = Doneshooting { unDoneshooting :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
