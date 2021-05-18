module Lib.Data.Types where

import           Servant.Docs

instance ToSample Text where
  toSamples Proxy = singleSample "Example Text"
