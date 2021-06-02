module Lib.Data.DagsdatoBackup where


import           Data.Aeson

newtype DagsdatoBackup = DagsdatoBackup { unDagsdatoBackup :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
