module Lib.Server.Protected.AccessKey.Types
    ( AccessKeyInfo(..)
    , AccessKeyUUID
    , AddAccessKey(..)
    , AccessKeyCreated(..)
    )
where
import Lib.Server.Protected.Types
import qualified Data.UUID as UUID
import Servant.Docs
import           Lib.Server.Types               ( Permission )
import           Data.Aeson
import           Data.Set                       ( Set )
import           Data.Time
import           Data.UUID.Typed

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text.Encoding as TE


import System.Random
import System.IO.Unsafe

type AccessKeyUUID = UUID AccessKey

data AccessKey

newtype AccessKeySecret
  = AccessKeySecret ByteString
    deriving (Show, Eq, Ord)
    deriving Generic


instance ToSample AccessKeySecret where
  toSamples Proxy = singleSample $ unsafePerformIO generateRandomAccessKeySecret

generateRandomAccessKeySecret :: IO AccessKeySecret
generateRandomAccessKeySecret = AccessKeySecret . SB.pack <$> replicateM 16 randomIO

instance FromJSON AccessKeySecret where
  parseJSON =
    withText "AccessKeySecret" $ \t ->
      case parseAccessKeySecretText t of
        Nothing -> fail "Invalid AccessKeySecret"
        Just aks -> pure aks

instance ToJSON AccessKeySecret where
  toJSON = toJSON . accessKeySecretText

accessKeySecretText :: AccessKeySecret -> Text
accessKeySecretText (AccessKeySecret bs) = TE.decodeUtf8 $ SB16.encode bs

parseAccessKeySecretText :: Text -> Maybe AccessKeySecret
parseAccessKeySecretText t =
  case SB16.decode $ TE.encodeUtf8 t of
    (d, "") -> Just $ AccessKeySecret d
    _ -> Nothing


data AccessKeyInfo
  = AccessKeyInfo
      { accessKeyInfoUUID :: AccessKeyUUID
      , accessKeyInfoName :: Text
      , accessKeyInfoCreatedTimestamp :: UTCTime
      , accessKeyInfoPermissions :: [Permission]
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)



instance ToSample AccessKeyInfo

data AddAccessKey
  = AddAccessKey
      { addAccessKeyName :: Text
      , addAccessKeyPermissions :: [Permission]
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

instance ToSample AddAccessKey

data AccessKeyCreated
  = AccessKeyCreated
      { accessKeyCreatedCreatedTimestamp :: UTCTime
      , accessKeyCreatedKey :: AccessKeySecret
      , accessKeyCreatedUUID :: AccessKeyUUID
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

instance ToSample AccessKeyCreated

instance ToSample UTCTime where
      toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample (UUID a) where
  toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)
