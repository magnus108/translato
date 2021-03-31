{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Lib (
    TextEntry, entry, userText,
    myBox
           , listBox
           ,brah
           ,brah2
           , kv
    ) where
import Control.Comonad.Hoist.Class

import           Control.Conditional            ( (?<>) )
import Utils.ListZipper (ListZipper)
import qualified Utils.ListZipper as ListZipper
import qualified Control.Comonad.Env as Env
import qualified Data.Map                          as Map

import Model
import Data.Generics.Labels
import           Options.Generic

import qualified Data.Map.Strict               as M
import           Control.Comonad hiding ((<@>))
import Control.Comonad.Store hiding ((<@>))
import qualified Control.Comonad.Store as Store

import           Control.Lens                   ( (^.), (.~))
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

kv :: (ComonadStore Position w) => w Translation -> (String, String)
kv w = (unPosition (Store.pos w), unTranslation (extract w))


brah :: (ComonadStore Position w, ComonadStore Status (t w), ComonadTrans t) => Translation -> t w Translation -> t w Translation
brah translation run =
    let position = pos (Store.lower run)
        status = pos run
        translations = status ^. #translations . #unTranslations
        translations' = Translations $ M.insert (unPosition position) translation translations
        status' = status & #translations .~ translations'
    in
        Store.seek (status') run

brah2 :: (ComonadStore Position w, ComonadHoist t, ComonadStore Status (t w), ComonadTrans t) => Position -> t w Translation -> t w Translation
brah2 position run = cohoist (Store.seek position) run


brah3 :: (ComonadStore Position w, ComonadStore Status (t w), ComonadTrans t) => t w (String,String) -> [(String,String)]
brah3 run =
    let wPosition = Store.lower run
        status = pos run
        elems = M.keys (status ^. #translations . #unTranslations)
    in
        Store.experiment (\position' -> fmap (Position) elems) wPosition


-------------------------------------------------------------------------------
myBox :: Behavior Run ->  Behavior (String -> Bool) -> UI (Element, Event String)
myBox bRun bFilter = do
    (eSelect, hSelection) <- liftIO newEvent

    let bItems = (\p -> filter (p . fst) . brah3 . extend (kv . lower) . unRun) <$> bFilter <*> bRun

    let bDisplay = pure $ \(x,y) -> do
            button <- UI.button
           --         #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    # set text x

            UI.on UI.click button $ \_ -> do
                    liftIO $ hSelection x

            UI.div #+ [element button, UI.string y]

    list <- UI.div # sink items (fmap <$> bDisplay <*> bItems)

    return (list, eSelect)

------------------------------------------------------------------------------
listBox :: Show a => Behavior (ListZipper a) -> UI (Element, Event (ListZipper a))
listBox xs = do
    (eSelect, hSelection) <- liftIO newEvent

    let displayOpen center items = do
            button <- UI.button 
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    # set text (show (extract items))
            UI.on UI.click button $ \_ -> do
                    liftIO $ hSelection items
            return button

    list <- UI.div #. "buttons has-addons" # sink items (ListZipper.toList . ListZipper.bextend displayOpen <$> xs)

    return (list, eSelect)
-------------------------------------------------------------------------------
items :: ReadWriteAttr Element [UI Element] ()
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
