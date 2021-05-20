module Lib.Utils where

import           System.FilePath
import           System.IO.Error
import           Data.Aeson
import qualified Data.ByteString               as B
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )

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
