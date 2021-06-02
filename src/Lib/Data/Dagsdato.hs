module Lib.Data.Dagsdato where


import           Data.Aeson

newtype Dagsdato = Dagsdato { unDagsdato :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)
