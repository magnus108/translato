module Lib.Data.LoginForm where

import           Data.Aeson
import           Servant.Docs
import           Lib.Data.Types


newtype Username
  = Username
      { usernameText :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

data LoginForm
  = LoginForm
      { loginFormUsername :: Username
      , loginFormPassword :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

instance ToSample LoginForm
instance ToSample Username
