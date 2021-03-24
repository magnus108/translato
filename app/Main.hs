{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

{-# LANGUAGE RecursiveDo #-}

module Main where

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

-------------------------------------------------------------------------------
data Config = Config { port :: Int }
    deriving Generic
    deriving anyclass ParseRecord


main :: IO ()
main = do
    Config (port) <- getRecord "Run"

    let static    = "static"
    let index     = "index.html"

    let translations = Translations $ M.fromList [("title", "Translations!"), ("lol", "fuck"), ("loller", "lollo")]
    let status = Status Normal "lol" Danish translations

    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           } $ setup status


    {-

    return window # sink title (lookup "title" . unRun <$> bRun)

    let bSelection = (\x -> Just x) <$> (currentK . unRun <$> bRun)
    listBox     <- UI.listBox  bListBoxItems bSelection (pure $ \x -> UI.string x)

    filterEntry <- UI.entry bFilterString
    key <- UI.span # sink text (currentK . unRun <$> bRun)
    value <- UI.span # sink text (currentV . unRun <$> bRun)

    getBody window #+ [ grid
        [[element key]
        ,[element value]
        ,[element filterEntry]
        ,[element listBox]
        ]]

    let userTextFilterEntry = UI.userText filterEntry
    bFilterString <- stepper "" $ rumors userTextFilterEntry
    let tFilter = isPrefixOf <$> userTextFilterEntry
        bFilter = facts  tFilter
        eFilter = rumors tFilter

    let eSelection  = filterJust $ rumors $ UI.userSelection listBox

    let bListBoxItems = (\p -> filter p . M.keys . unTranslations)
                            <$> bFilter <*> bTranslations
    --let bListBoxItems :: Behavior [DatabaseKey]
        --bListBoxItems = (\p show -> filter (p . show) . M.keys . unTranslations)
    --                <$> bFilter <*> bShowDataItem <*> bTranslations



    bTranslations <- stepper translations UI.never
            -- $ head . NE.fromList <$> unions [ (\translations status translation -> Translations $ M.adjust (const translation) (status ^. #position) (unTranslations translations)) <$> bTranslations <*> bStatus <@> eTranslation ]

    bStatus <- stepper status 
        $ head . NE.fromList <$> unions [ (\status search -> Lens.set #position search status) <$> bStatus <@> eSelection ]

    let bRun = (\status translations -> Run $ EnvT
            (status ^. #style)
            (store (\translation -> M.findWithDefault translation translation (translations ^. #unTranslations))
                   (status ^. #position)
            )) <$> bStatus <*> bTranslations


-}
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

    {-
    bTranslations <- stepper (Translations $ M.fromList [("title", "Translations!"), ("lol", "fuck"), ("loller", "lollo")]) UI.never
            -- $ head . NE.fromList <$> unions [ (\translations status translation -> Translations $ M.adjust (const translation) (status ^. #position) (unTranslations translations)) <$> bTranslations <*> bStatus <@> eTranslation ]

    bStatus <- stepper (Status Normal "lol" Danish) UI.never
            -- $ head . NE.fromList <$> unions [ (\status search -> Lens.set #position search status) <$> bStatus <@> eSearch ]

    let bRun = (\status translations -> Run $ EnvT
            (status ^. #style)
            (store (\translation -> M.findWithDefault translation translation (translations ^. #unTranslations))
                   (status ^. #position)
            )) <$> bStatus <*> bTranslations
            -}

{-
    let eSelection  = rumors $ UI.userSelection listBox

    bSelection <- stepper Nothing $ head <$> unions
        [ eSelection
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelection <*> bShowDataItem <@> eFilter
        ]

-}

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
setup :: Status -> Window -> UI ()
setup status window = void $ mdo
    return window # sink title (Store.peeks (Lens.set #position "title") . unRun <$> bRun)

    key <- UI.span # sink text (position . Store.pos . unRun <$> bRun)
    value <- UI.span # sink text (extract . unRun <$> bRun)

    listBox <- Lib.listBox bRun

    getBody window #+ [ grid
        [[element key]
        ,[element value]
        ,[UI.hr]
        --,[element filterEntry]
        ,[element listBox]
        ]]

    let run = Run $ EnvT
            (status ^. #style)
            (store (\status' -> M.findWithDefault (status' ^. #position) (status' ^. #position) (status' ^. #translations . #unTranslations))
                   status
            )

    bRun <- stepper run UI.never

    xx <- extend Lib.kv . unRun <$> currentValue bRun
    traceShowM (extract xx)

    return ()
