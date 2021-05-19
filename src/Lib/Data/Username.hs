module Lib.Data.Username where

import           Data.Aeson
import           Servant.Docs

newtype Username
  = Username
      { usernameText :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

