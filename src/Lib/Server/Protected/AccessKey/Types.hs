module Lib.Server.Protected.AccessKey.Types
    ( AccessKeyInfo(..)
    , AccessKeyUUID
    , AddAccessKey(..)
    , AccessKeyCreated(..)
    )
where

import           Lib.Server.Types               ( Permission )
import           Data.Aeson
import           Data.Set                       ( Set )
import           Data.Time
import           Data.UUID.Typed

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text.Encoding as TE

type AccessKeyUUID = UUID AccessKey

data AccessKey

newtype AccessKeySecret
  = AccessKeySecret ByteString
    deriving (Show, Eq, Ord)
    deriving Generic

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
      , accessKeyInfoPermissions :: Set Permission
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data AddAccessKey
  = AddAccessKey
      { addAccessKeyName :: Text
      ,   addAccessKeyPermissions :: Set Permission
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

data AccessKeyCreated
  = AccessKeyCreated
      { accessKeyCreatedCreatedTimestamp :: UTCTime
      , accessKeyCreatedKey :: AccessKeySecret
      , accessKeyCreatedUUID :: AccessKeyUUID
      }
    deriving (Show, Eq, Ord)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

