module Lib.Config
    ( Config(..)
    , loadConfig
    , readJSONFileStrict
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

data Config = Config
    { dumpFile :: !FilePath
    , photographersFile :: !FilePath
    }
    deriving (Show, Eq)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


readJSONFileStrict :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFileStrict fp = liftIO $ do
    bs <- B.readFile fp
    case eitherDecodeStrict' bs of
        Left  e -> throwM $ userError e
        Right x -> return x


loadConfig :: MonadIO m => FilePath -> FilePath -> m Config
loadConfig root file = do
    let config = root </> file
    readJSONFileStrict config

