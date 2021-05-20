module Lib.Config
    ( Config(..)
    , loadConfig
    )
where

import           System.FilePath
import           Lib.App
import           Data.Aeson
import qualified Data.ByteString               as B
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           System.IO.Error
import           Lib.Utils

data Config = Config
    { dumpFile :: !FilePath
    , photographersFile :: !FilePath
    }
    deriving (Show, Eq)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


loadConfig :: MonadIO m => FilePath -> FilePath -> m Config
loadConfig root file = do
    let config = root </> file
    readJSONFileStrict config

