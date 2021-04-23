module Lib.Config
    ( Config(..)
    , loadConfig
    )
where

import           System.FilePath
import           Lib.App
import           Data.Aeson


data Config = Config
    { dumpFile :: !FilePath
    } deriving (Show, Eq)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


loadConfig :: MonadIO m => FilePath -> FilePath -> m Config
loadConfig root file = do
    let config = root </> file
    return $ Config "lol"
    --readJSONFile config

