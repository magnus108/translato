module Text (content) where

import Model

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Format

content :: Behavior Run -> UI Element
content bRun = UI.div #+
                [ UI.p # sink presentation (Format.lookup "text3" <$> bRun)
                , UI.p # sink presentation (Format.lookup "text2" <$> bRun)
                , UI.p # sink presentation (Format.lookup "title" <$> bRun)
                ]
