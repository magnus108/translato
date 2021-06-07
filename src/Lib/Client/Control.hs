{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

module Lib.Client.Control where


import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           GHC.Generics
import           GHC.OverloadedLabels         (IsLabel (..))
import           Options.Generic 
import           Data.Generics.Labels


import qualified Lib.Client.FilePicker         as FilePicker
import qualified Lib.Client.Select             as Select
import qualified Foreign.JavaScript            as JS
import qualified Relude.Unsafe                 as Unsafe
import           Servant.Auth.Client
import           Control.Conditional            ( (?<>) )
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Lib.Client.Types
import           Lib.Utils
import           Servant                 hiding ( Handler )

import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import           Web.Cookie                     ( parseSetCookie
                                                , setCookieName
                                                , SetCookie(..)
                                                , setCookieValue
                                                )

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )

import           Servant.Auth.Client
import           Lib.Data.Photographer          ( Photographers )
import           Graphics.UI.Threepenny.Core
import           Servant                 hiding ( throwError
                                                , Handler
                                                )
import qualified Servant.Client                as Servant

import           Lib.Client.Error               ( ClientAppError
                                                , throwError
                                                , ClientAppErrorType(..)
                                                )
import           GHC.Base                       ( failIO )

import qualified Control.Monad.Except          as E

import           Lib.Utils
import           Lib.Api.Types
import qualified Utils.ListZipper              as ListZipper
import qualified Control.Lens                  as Lens
import qualified Lib.Data.Photographer         as Photographer
import qualified Lib.Data.Tab                  as Tab
import qualified Lib.Data.Session              as Session
import qualified Lib.Data.Grade                as Grade
import Lib.Data.Grade                
import qualified Lib.Data.Location             as Location
import qualified Lib.Data.Shooting             as Shooting
import qualified Lib.Data.Dump                 as Dump
import qualified Lib.Data.Camera               as Camera
import qualified Lib.Data.Doneshooting         as Doneshooting
import qualified Lib.Data.Dagsdato             as Dagsdato
import qualified Lib.Data.DagsdatoBackup       as DagsdatoBackup
import           Control.Comonad         hiding ( (<@)
                                                , (<@>)
                                                )
import           Lib.Client.Utils
import qualified Lib.Client.Text               as Text
import Control.Lens ((<>~), (.~))

mkControls :: a -> Behavior (Maybe (ListZipper.ListZipper a)) -> ClientApp (Element, Event (Maybe (ListZipper.ListZipper a)), Event (Maybe (ListZipper.ListZipper a)))
mkControls sempty bXs = do
    insert <- liftUI $ UI.button #. "button" # set text "add"
    delete <- liftUI $ UI.button #. "button" # set text "delete"
    control <- liftUI $ UI.div #. "buttons has-addons" # set children [insert, delete]

    let eInsert = fmap (\xs -> ListZipper.add sempty xs) <$> bXs <@  UI.click insert
    let eDelete = fmap (\xs -> ListZipper.remove xs) <$> bXs <@  UI.click delete

    return (control, eInsert, eDelete)

