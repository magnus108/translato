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

import Data.Generics.Sum.Constructors (_Ctor)
import           Control.Lens                   ( (^.), (^?))
import qualified Control.Lens as Lens
import           Options.Generic

import qualified Control.Comonad.Store as Store


import qualified Data.List.NonEmpty as NE
import           Control.Comonad hiding ((<@>))
import           Control.Comonad.Trans.Store

import qualified Utils.ListZipper as ListZipper

import qualified Data.Map.Strict               as M

import qualified Lib
import qualified Lib2
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (title, grid, column, row)
import Reactive.Threepenny
import Columns3 

-------------------------------------------------------------------------------
data Config = Config { port :: Int }
    deriving Generic
    deriving anyclass ParseRecord


main :: IO ()
main = do
    Config (port) <- getRecord "Run"
    Lib.main port ""

    {-
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           , jsCallBufferMode           = NoBuffering
                           } $ setup position status
                           -}



setup :: Position -> Status -> Window -> UI ()
setup position status window = void $ mdo
    _ <- return window # sink title (Format.lookup "title" ("lol" :: String) ("loL2" :: String) <$> bRun)

    key <- UI.span # sink text (unPosition . Store.pos . Store.lower . unRun <$> bRun)
    value <- UI.span # sink text (unTranslation . extract . unRun <$> bRun)

    (myBox, eKeyChange) <- Lib2.myBox bRun bFilter
    filterEntry <- Lib2.entry bFilterString
    changeEntry <- Lib2.entry (unTranslation . extract . unRun <$> bRun)


    let getTrans' u
                | Just _ <- u ^? _Ctor @"Danish" = "danish"
                | Just _ <- u ^? _Ctor @"English" = "english"
                | True = error "Fuck"

    (languageSelection, eLanguageSelection) <- Lib2.listBox bRun getTrans' (languages . Store.pos . unRun <$> bRun)

------------------------------------------------------------------------------
    contentA <- UI.div # set text "bob"
    contentB <- UI.button # set text "boby"

    (eStyleSelection, hStyleSelection) <- liftIO newEvent

    content <- UI.div

    -- HOW?
    let problemName = \case
            Normal -> "normal"
            Translating -> "translating"


    let setContent f run' hStyleSelection' = do
            let styles' = styles $ Store.pos $ unRun $ run'
            menu <- UI.div #. "buttons has-addons" #+ (ListZipper.toList (ListZipper.bextend (Lib2.displayOpen f run hStyleSelection') styles'))
            let focus = extract styles'
            case focus of
                Normal -> element content # set children [menu, contentA]
                Translating -> element content # set children [menu, contentB]

    --GENBRUG
    liftIOLater $ runUI window $ void $ do
        run' <- currentValue bRun
        setContent problemName run' hStyleSelection

    liftIOLater $ onChange bRun $ \run' -> runUI window $ void $ do
            setContent problemName run' hStyleSelection
------------------------------------------------------------------------------

    _ <- getBody window #+ [UI.div #. "container" #+
        (construct $ Grid
            [ return $ Row [S $ element languageSelection]
            , return $ Row [S $ UI.hr]
            , return $ Row [M (Grouped [S' $ mkPresentation bRun "key", S' $ element key])]
            , return $ Row [M (Grouped [S' $ UI.string "value: ", S' $ element value])]
            , return $ Row [S $ UI.hr]
            , return $ Row [M (Grouped [S' $ UI.string "change it: ", S' $ element changeEntry])]
            , return $ Row [S $ UI.hr]
            , return $ Row [M (Grouped [S' $ UI.string "filter ", S' $ element filterEntry])]
            , return $ Row [SS $ element myBox]
            , return $ Row [S $ UI.hr]
            , return $ Row [S $ element content]
            ])
        ]


    let userTextFilterEntry = Lib2.userText filterEntry
    bFilterString <- stepper "" $ rumors userTextFilterEntry
    let tFilter = isPrefixOf <$> userTextFilterEntry
        bFilter = facts  tFilter
        _ = rumors tFilter

    let userTextChangeEntry = Lib2.userText changeEntry
        eDataItemChange = rumors $ userTextChangeEntry


    let getTrans u
                | Just v <- u ^? _Ctor @"Danish"= v
                | Just v <- u ^? _Ctor @"English" = v
                | True = error "fuck"

    let run = Run $ StoreT
            ( store (\position' status' -> M.findWithDefault (Translation (unPosition position')) (unPosition position') (unTranslations (getTrans (extract (status' ^. #languages))))) position ) status



-------------------------------------------------------------------------------
    -- lave events og handlers
    -- put events ind i bRun
    -- register handles på all events
    -- lyt til changes og hånter
    (eChange, _) <- liftIO $ newEvent
    bRun <- stepper run $ head . NE.fromList <$> unions
            [ (\run' translation' -> Run $ Lib2.brah (Translation translation') (unRun run')) <$> bRun <@> eDataItemChange -- move this out and  handle with register
            , (\run' language' -> Run $ Store.seeks (Lens.set #languages language') (unRun run')) <$> bRun <@> eLanguageSelection
            , (\run' style' -> Run $ Store.seeks (Lens.set #styles style') (unRun run')) <$> bRun <@> eStyleSelection
            , (\run' position' -> Run $ Lib2.brah2 (Position position') (unRun run')) <$> bRun <@> eKeyChange
            , (\run' position' -> Run $ Lib2.brah2 (Position position') (unRun run')) <$> bRun <@> eChange
            ]
-------------------------------------------------------------------------------

    --  how do i save bRun?
    -- eChange
    -- onEvent eDataItemChange hChange
    -- register?

    return ()

