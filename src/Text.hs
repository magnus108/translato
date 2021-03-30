module Text (content) where

import qualified Lib
import Model

import Data.Generics.Labels
import           Options.Generic
import           Safe                           ( atMay )
import           Control.Lens                   ( (^.) )
import qualified Control.Lens as Lens
import           Options.Generic

import qualified Control.Comonad.Store.Class   as Store

import           Control.Comonad
import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env

import qualified Data.Map.Strict               as M

import qualified Lib
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

content :: Behavior Run -> UI Element
content bRun = UI.div #+
                [ UI.p # sink presentation (Lib.lookup "text3" <$> bRun)
                , UI.p # sink presentation (Lib.lookup "text2" <$> bRun)
                , UI.p # sink presentation (Lib.lookup "title" <$> bRun)
                ]



presentationtext :: WriteAttr Element String
presentation = mkWriteAttr $ \s el ->
    runFunction $ ffi "$(%1).text(%2)" el s


mkPresentation :: String -> UI Element
mkPresentation s = mkElement "span" # set text s
