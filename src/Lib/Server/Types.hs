module Lib.Server.Types
    ( AppServer
    , ToApi
    )
where

import           Servant.API                    ( Header'
                                                , Required
                                                , Strict
                                                )
import           Servant.API.Generic            ( ToServantApi )
import           Servant.Server.Generic         ( AsServerT )

import           Lib.App                        ( App )


type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

type RequiredHeader = Header' '[Required, Strict]
