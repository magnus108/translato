{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Main where

import qualified Lib

import           Safe                           ( atMay )
import Data.Generics.Labels
import           Control.Lens                   ( (^.) )
import           Options.Generic
import           Data.Aeson

import           Control.Comonad
import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env
import           Control.Comonad.Store.Class
import qualified Control.Comonad.Store.Class   as Store
import           Control.Comonad.Env.Class
import qualified Control.Comonad.Env.Class     as Env


import qualified Data.Map.Strict               as M


-------------------------------------------------------------------------------
data Config = Config { port :: Int }
    deriving Generic
    deriving anyclass ParseRecord

data Style = Translating | Normal
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

data Translations = Translations { unTranslations :: M.Map String String }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

data State = State { style :: Style
                   , position :: String
                   }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

data Run = Run { unRun :: EnvT Style (Store String) (Maybe String) }

-------------------------------------------------------------------------------
{-
getPosition :: (ComonadStore String w) => w a -> String
getPosition = Store.pos

getStyle :: (ComonadEnv Style w) => w a -> Style
getStyle = Env.ask
-}
-------------------------------------------------------------------------------

main :: IO ()
main = do
    Config (port) <- getRecord "Run"

    let (Translations translations) =
            Translations $ M.fromList [("lol", "fuck"), ("loller", "lollo")]
    let state = State Normal "lol"

    let (Run run) = Run $ EnvT
            (state ^. #style)
            (store (\translation -> M.lookup translation translations)
                   (state ^. #position)
            )

    Lib.run port

-------------------------------------------------------------------------------
lookup :: (ComonadStore String w) => w (Maybe String) -> Maybe String
lookup w = Store.seek "loller" w & extract

-------------------------------------------------------------------------------
format' :: String -> [String] -> String
format' code args = go code
  where
    at xs i = maybe " " id $ atMay xs i
    argument i = args `at` i

    go []               = []
    go ('%' : '%' : cs) = '%' : go cs
    go ('%' : c   : cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c : cs) = c : go cs

format :: PrintfType a => String -> a
format x = fancy (format' x)

class PrintfType a where
    fancy :: ([String] -> String) -> a

instance PrintfType String where
    fancy f = f []

instance (PrintfType r) => PrintfType (String -> r) where
    fancy f x = fancy $ \xs -> f (x : xs)
-------------------------------------------------------------------------------
