module Lib.Utils where

import qualified Data.ByteString.Lazy          as BS
import           System.IO.Error
import           Data.Aeson
import qualified Data.ByteString               as B
import           Control.Monad.Catch            ( throwM
                                                )

import           Control.Exception              ( finally )

class Has field env where
    obtain :: env -> field

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field


readJSONFileStrict :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFileStrict fp = liftIO $ do
    bs <- B.readFile fp
    case eitherDecodeStrict' bs of
        Left  e -> throwM $ userError e
        Right x -> return x

writeJSONFile :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJSONFile fp item = liftIO $ BS.writeFile fp (encode item)


type WithIt r m s = (MonadReader r m, MonadIO m, Has s r)

class FilePathable a where
    toFilePath :: a -> MVar FilePath

writeThing :: forall  r m s a . (ToJSON a, WithIt r m s, FilePathable s) => s -> a -> m ()
writeThing s a = do
    mFile <- toFilePath <$> grab @s
    file  <- liftIO $ takeMVar mFile
    liftIO $ writeJSONFile file a `finally` (putMVar mFile file)



