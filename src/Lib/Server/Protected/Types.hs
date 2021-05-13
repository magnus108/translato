module Lib.Server.Protected.Types where

import Servant.API
import Servant.API.Generic
import Servant.Auth.Docs ()
import Servant.Docs

instance ToSample Text where
  toSamples Proxy = singleSample "Example Text"
