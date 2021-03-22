module Model where

import qualified Data.Map.Strict               as M


import           Options.Generic
import           Data.Generics.Labels
import           Data.Aeson
import           Control.Comonad         hiding ( (<@>) )
import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env
import           Control.Comonad.Store.Class
import qualified Control.Comonad.Store.Class   as Store
import           Control.Comonad.Env.Class
import qualified Control.Comonad.Env.Class     as Env

data Run = Run { unRun :: EnvT Style (Store (String, Translations)) String }

data Translations = Translations { unTranslations :: M.Map String String }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Style = Translating | Normal
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Status = Status { style :: Style
                   , position :: String
                   , language :: Language
                   }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Language = Danish | English
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


