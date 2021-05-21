module Lib.Client where

import           Graphics.UI.Threepenny.Core
import           Lib.Client.Types

setup :: Window -> ClientApp ()
setup win = do
    liftUI $ return win # set title "test"
    return ()
