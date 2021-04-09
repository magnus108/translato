module Model where

import           Data.Aeson

import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env

import qualified Data.Map.Strict               as M

import           Utils.ListZipper               ( ListZipper )
import qualified Utils.ListZipper              as ListZipper



data Run = Run { unRun :: StoreT Status (Store Position) Translation }


data Translation = Translation { unTranslation :: String }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

data Translations = Translations { unTranslations :: M.Map String Translation }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Style = Translating | Normal
    deriving (Show, Eq)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Position = Position { unPosition :: String }
    deriving (Show, Eq)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

data Status = Status { languages :: ListZipper Language
                     , styles :: ListZipper Style
                     }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Language = Danish Translations | English Translations
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)
