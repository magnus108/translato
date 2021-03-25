module Model where

import           Data.Aeson

import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env

import qualified Data.Map.Strict               as M

import Utils.ListZipper (ListZipper)
import qualified Utils.ListZipper as ListZipper

data Run = Run { unRun :: EnvT (ListZipper Style) (Store Status) String }

data Translations = Translations { unTranslations :: M.Map String String }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Style = Translating | Normal
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Status = Status { position :: String
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


