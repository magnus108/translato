module Lib.Config
    ( Config(..)
    , loadConfig
    )
where

import           System.FilePath
import           Data.Aeson
import           Lib.Utils

data Config = Config
    { dumpFile :: !FilePath
    , dagsdatoFile :: !FilePath
    , dagsdatoBackupFile :: !FilePath
    , photographersFile :: !FilePath
    , tabsFile :: !FilePath
    }
    deriving (Show, Eq)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


loadConfig :: MonadIO m => FilePath -> FilePath -> m Config
loadConfig root file = do
    let config = root </> file
    readJSONFileStrict config

