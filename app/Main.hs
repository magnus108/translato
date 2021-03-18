{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Lib

import Data.Generics.Labels

import           Safe                           ( atMay )
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

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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

data Run = Run { unRun :: EnvT Style (Store String) String }


main :: IO ()
main = do
    Config (port) <- getRecord "Run"

    let static    = "static"
    let index     = "index.html"

    let (Translations translations) =
            Translations $ M.fromList [("title", "Translations!"), ("lol", "fuck"), ("loller", "lollo")]

    let state = State Normal "lol"

    let (Run run) = Run $ EnvT
            (state ^. #style)
            (store (\translation -> M.findWithDefault translation translation translations)
                   (state ^. #position)
            )

    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           } $ setup (Run run)

    Lib.run port

-------------------------------------------------------------------------------
setup :: Run -> Window -> UI ()
setup (Run run) window = void $ mdo
    return window # set title (lookup "title" run)

    translationEntry <- UI.entry bTranslation

    getBody window #+ [ grid
        [[row [string "Translation:", element translationEntry]]
        ]]

    bTranslation <- stepper "" . rumors $ UI.userText translationEntry



    return ()

-------------------------------------------------------------------------------
lookup :: (ComonadStore String w) => String -> w String -> String
lookup k w = Store.seek k w & extract

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
