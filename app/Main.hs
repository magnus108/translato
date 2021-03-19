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

import qualified Data.List.NonEmpty as NE
import           Safe                           ( atMay )
import           Control.Lens                   ( (^.) )
import qualified Control.Lens                  as Lens
import           Options.Generic
import           Data.Aeson

import           Control.Comonad hiding ((<@>))
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

data Run = Run { unRun :: EnvT Style (Store String) String }


main :: IO ()
main = do
    Config (port) <- getRecord "Run"

    let static    = "static"
    let index     = "index.html"



    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           } setup

    Lib.run port

-------------------------------------------------------------------------------
setup :: Window -> UI ()
setup window = void $ mdo

    return window # sink title (lookup "title" . unRun <$> bRun)


    {-
    key <- UI.span # sink text bSearchString
    value <- UI.span # sink text bTranslationString

    searchEntry <- UI.entry bSearchString
    translationEntry <- UI.entry bTranslationString

    listBox     <- UI.listBox  bListBoxItems bSelection bDisplayDataItemp

    getBody window #+ [ grid
        [[row [string "Search translation:", element searchEntry]]
        ,[element key]
        ,[row [string "Change translation:", element translationEntry]]
        ,[element value]
        ,[element listBox]
        ]]

    let tSearch = UI.userText searchEntry
        bSearch = facts  tSearch
        eSearch = rumors tSearch

    let tTranslation = UI.userText translationEntry
        bTranslation = facts  tTranslation
        eTranslation = rumors tTranslation

    let bSearchString = currentK . unRun <$> bRun
    let bTranslationString = currentV . unRun <$> bRun
    -}

    bTranslations <- stepper (Translations $ M.fromList [("title", "Translations!"), ("lol", "fuck"), ("loller", "lollo")]) UI.never
            -- $ head . NE.fromList <$> unions [ (\translations status translation -> Translations $ M.adjust (const translation) (status ^. #position) (unTranslations translations)) <$> bTranslations <*> bStatus <@> eTranslation ]

    bStatus <- stepper (Status Normal "lol" Danish) UI.never
            -- $ head . NE.fromList <$> unions [ (\status search -> Lens.set #position search status) <$> bStatus <@> eSearch ]

    let bRun = (\status translations -> Run $ EnvT
            (status ^. #style)
            (store (\translation -> M.findWithDefault translation translation (translations ^. #unTranslations))
                   (status ^. #position)
            )) <$> bStatus <*> bTranslations

{-
    let eSelection  = rumors $ UI.userSelection listBox

    bSelection <- stepper Nothing $ head <$> unions
        [ eSelection
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelection <*> bShowDataItem <@> eFilter
        ]

-}

    return ()

-------------------------------------------------------------------------------
lookup :: (ComonadStore String w) => String -> w String -> String
lookup k w = Store.peek k w

currentK :: (ComonadStore String w) => w String -> String
currentK = Store.pos

currentV :: (ComonadStore String w) => w String -> String
currentV = extract


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
