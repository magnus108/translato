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


import qualified Data.List.NonEmpty as NE
import           Control.Comonad hiding ((<@>))
import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env

import qualified Data.Map.Strict               as M

import qualified Lib
import qualified Text as T
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

    --(eChange, hChange) <- newEvent

    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           } $ setup status



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

    listBox <- Lib.listBox bRun bFilter
    filterEntry <- Lib.entry bFilterString
    changeEntry <- Lib.entry (extract . unRun <$> bRun)

    myText <- T.content bRun

    getBody window #+ [ grid
        [[row [UI.string "key: ", element key]]
        ,[row [UI.string "value: ", element value]]
        ,[UI.hr]
        ,[row [UI.string "change it: ", element changeEntry]]
        ,[UI.hr]
        ,[row [UI.string "filter ",  element filterEntry]]
        ,[element listBox]
        ,[UI.hr]
        ,[element myText]
        ]]

    let userTextFilterEntry = Lib.userText filterEntry
    bFilterString <- stepper "" $ rumors userTextFilterEntry
    let tFilter = isPrefixOf <$> userTextFilterEntry
        bFilter = facts  tFilter
        eFilter = rumors tFilter

    let userTextChangeEntry = Lib.userText changeEntry
        eDataItemChange = rumors $ userTextChangeEntry


    let run = Run $ EnvT
            (status ^. #style)
            (store (\status' -> M.findWithDefault (status' ^. #position) (status' ^. #position) (status' ^. #translations . #unTranslations))
                   status
            )

    bRun <- stepper run $ head . NE.fromList <$> unions
            [ (\run translation -> Run $ Lib.brah translation (unRun run)) <$> bRun <@> eDataItemChange ]


    --  how do i save bRun?
    -- eChange
    -- onEvent eDataItemChange hChange
    -- register?

    return ()

