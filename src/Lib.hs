{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE GADTs          #-}

module Lib (
    TextEntry, entry, userText,
    myBox
           , listBox
           ,brah
           ,brah2
           , kv
           , displayOpen
    ) where
import Control.Comonad.Hoist.Class
import Data.Generics.Sum.Constructors (_Ctor)

import           Control.Conditional            ( (?<>) )
import Utils.ListZipper (ListZipper)
import qualified Utils.ListZipper as ListZipper
import qualified Control.Comonad.Env as Env
import qualified Data.Map                          as Map

import Format
import Model
import Data.Generics.Labels
import           Options.Generic

import qualified Data.Map.Strict               as M
import           Control.Comonad hiding ((<@>))
import Control.Comonad.Store hiding ((<@>))
import qualified Control.Comonad.Store as Store

import           Control.Lens                   ( (^.), (.~), (^?))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (title)
import Reactive.Threepenny

-------------------------------------------------------------------------------
data TextEntry = TextEntry
    { _elementTE :: Element
    , _userTE    :: Tidings String
    }

instance Widget TextEntry where getElement = _elementTE

userText :: TextEntry -> Tidings String
userText = _userTE

entry
    :: Behavior String
    -> UI TextEntry
entry bValue = do

    input <- UI.input

    bEditing <- stepper False $ and <$>
        unions [True <$ UI.focus input, False <$ UI.blur input]

    window <- askWindow

    liftIOLater $ runUI window $ void $ do
        current <- currentValue bValue
        element input # set value current

    liftIOLater $ onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    let _elementTE = input
        _userTE    = tidings bValue $ UI.valueChange input
    return TextEntry {..}

-------------------------------------------------------------------------------


brah :: (ComonadStore Position w, ComonadStore Status (t w), ComonadTrans t) => Translation -> t w Translation -> t w Translation
brah translation run =
    let position = pos (Store.lower run)
        status = pos run
        languages = status ^. #languages
        language = languages & extract

        getTrans u
                | Just v <- u ^? _Ctor @"Danish"= Danish $ Translations $ M.insert (unPosition position) translation (unTranslations v)
                | Just v <- u ^? _Ctor @"English" = English $ Translations $ M.insert (unPosition position) translation (unTranslations v)

        languages' = ListZipper.setter languages (getTrans language)
        status' =  status & #languages .~ languages'
    in
        Store.seek (status') run

brah2 :: (ComonadStore Position w, ComonadHoist t, ComonadStore Status (t w), ComonadTrans t) => Position -> t w Translation -> t w Translation
brah2 position run = cohoist (Store.seek position) run


brah3 :: (ComonadStore Position w, ComonadStore Status (t w), ComonadTrans t) => t w (String,String, Bool) -> [(String,String,Bool)]
brah3 run =
    let wPosition = Store.lower run
        status = pos run
        getTrans u
                | Just v <- u ^? _Ctor @"Danish"= v
                | Just v <- u ^? _Ctor @"English" = v

        elems = M.keys (unTranslations (getTrans (status ^. #languages & extract)))
    in
        Store.experiment (\position' -> fmap (Position) elems) wPosition


kv :: (ComonadStore Position w, ComonadStore Status (t w), ComonadTrans t) => t w Translation -> t w Translation -> (String, String, Bool)
kv w1 w2 = (unPosition (Store.pos (lower w2)), unTranslation (extract (lower w2)), Store.pos (lower w2) == Store.pos (lower w1))

extendi :: Comonad w => (w a -> w a -> b) -> w a -> w b
extendi f w = extend (f w) w

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-------------------------------------------------------------------------------
myBox :: Behavior Run ->  Behavior (String -> Bool) -> UI (Element, Event String)
myBox bRun bFilter = do
    (eSelect, hSelection) <- liftIO newEvent

    let bItems = (\p -> filter (p . fst3) . brah3 . extendi kv . unRun) <$> bFilter <*> bRun

    let bDisplay = pure $ \(x,y,center) -> do
            button <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    # set text x

            UI.on UI.click button $ \_ -> do
                    liftIO $ hSelection x

            -- HER
            UI.div #+ [element button, UI.string y]

    list <- UI.div # sink items (fmap <$> bDisplay <*> bItems)

    return (list, eSelect)

------------------------------------------------------------------------------
--GENBRUG
displayOpen f run handler center items = do
        button <- UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                # set presentation (lookup (f (extract items)) run)

        UI.on UI.click button $ \_ -> do
                liftIO $ handler items

        return button

listBox :: Behavior Run -> (a -> String) -> Behavior (ListZipper a) -> UI (Element, Event (ListZipper a))
listBox bRun f xs = do
    (eSelect, hSelection) <- liftIO newEvent

    list <- UI.div #. "buttons has-addons" # sink items ((\run ys -> ListZipper.toList (ListZipper.bextend (displayOpen f run hSelection) ys)) <$> bRun <*> xs)

    return (list, eSelect)
-------------------------------------------------------------------------------
items :: ReadWriteAttr Element [UI Element] ()
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
