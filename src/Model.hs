module Model where

import           Data.Aeson

import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env

import qualified Data.Map.Strict               as M


data Run = Run { unRun :: EnvT Style (Store Status) String }

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
                   , translations :: Translations
                   }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Language = Danish | English
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


