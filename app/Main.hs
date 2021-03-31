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

import Format
import qualified Format as Format

import Data.Generics.Labels
import           Options.Generic
import           Control.Lens                   ( (^.) )
import qualified Control.Lens as Lens
import           Options.Generic

import qualified Control.Comonad.Store as Store
import qualified Control.Comonad.Env as Env


import qualified Data.List.NonEmpty as NE
import           Control.Comonad hiding ((<@>))
import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env

import Utils.ListZipper (ListZipper)
import qualified Utils.ListZipper as ListZipper

import qualified Data.Map.Strict               as M

import qualified Lib
import qualified Text as T
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (title)

-------------------------------------------------------------------------------
data Config = Config { port :: Int }
    deriving Generic
    deriving anyclass ParseRecord


main :: IO ()
main = do
    Config (port) <- getRecord "Run"

    let static    = "static"
    let index     = "index.html"

    let translations = Translations $ M.fromList [("text3", Translation "bob%1"), ("title", Translation "Translations! %1 %2"), ("key", Translation "Key: "), ("lol", Translation "fuck"), ("loller", Translation "lollo")]
    let languages = ListZipper.ListZipper [] Danish [English]
    let styles = ListZipper.ListZipper [] Normal [Translating]
    let status = Status translations languages styles
    let position = Position "title"

    --(eChange, hChange) <- newEvent

    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           } $ setup position status


setup :: Position -> Status -> Window -> UI ()
setup position status window = void $ mdo
    return window # sink title (Format.lookup "title" ("lol" :: String) ("loL2" :: String) <$> bRun)

    key <- UI.span # sink text (unPosition . Store.pos . Store.lower . unRun <$> bRun)
    value <- UI.span # sink text (unTranslation . extract . unRun <$> bRun)

    (myBox, eKeyChange) <- Lib.myBox bRun bFilter
    filterEntry <- Lib.entry bFilterString
    changeEntry <- Lib.entry (unTranslation . extract . unRun <$> bRun)

    myText <- T.content bRun

    (styleSelection, eStyleSelection) <- Lib.listBox (styles . Store.pos . unRun <$> bRun)
    (languageSelection, eLanguageSelection) <- Lib.listBox (languages . Store.pos . unRun <$> bRun)

    getBody window #+ [ grid
        [[element styleSelection]
        ,[UI.hr]
        ,[element languageSelection]
        ,[UI.hr]
        ,[row [mkPresentation bRun "key" , element key]]
        ,[row [UI.string "value: ", element value]]
        ,[UI.hr]
        ,[row [UI.string "change it: ", element changeEntry]]
        ,[UI.hr]
        ,[row [UI.string "filter ",  element filterEntry]]
        ,[element myBox]
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


    let run = Run $ StoreT
            ( store (\position' status' -> M.findWithDefault (Translation (unPosition position')) (unPosition position') (status' ^. #translations . #unTranslations)) position ) status

    bRun <- stepper run $ head . NE.fromList <$> unions
            [ (\run translation -> Run $ Lib.brah (Translation translation) (unRun run)) <$> bRun <@> eDataItemChange
            , (\run language -> Run $ Store.seeks (Lens.set #languages language) (unRun run)) <$> bRun <@> eLanguageSelection
            , (\run style -> Run $ Store.seeks (Lens.set #styles style) (unRun run)) <$> bRun <@> eStyleSelection
            , (\run position -> Run $ Lib.brah2 (Position position) (unRun run)) <$> bRun <@> eKeyChange
            ]

    --  how do i save bRun?
    -- eChange
    -- onEvent eDataItemChange hChange
    -- register?

    return ()

