{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Lib (
    TextEntry, entry, userText,
    myBox
           , listBox
           ,brah
           , kv
    ) where


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
import qualified Control.Comonad.Store.Class   as Store

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

kv :: (ComonadStore Status w) => w Translation -> (String, String)
kv w = (position (Store.pos w), unTranslation (extract w))


brah :: (ComonadStore Status w) => Translation -> w Translation -> w Translation
brah translation run =
    let status = pos run
        position = status ^. #position
        translations = status ^. #translations . #unTranslations
        translations' = Translations $ M.insert position translation translations
        status' = status & #translations .~ translations'
    in
        Store.seeks (const status') run


-------------------------------------------------------------------------------
myBox :: Behavior Run ->  Behavior (String -> Bool) -> UI (Element, Event String)
myBox bRun bFilter = do
    (eSelect, hSelection) <- liftIO newEvent

    let bItems = (\p -> filter (p . fst) . Store.experiment (\status' -> let elems = (M.keys (status' ^. #translations . #unTranslations)) in fmap (\e -> status' & #position .~ e) elems) . extend kv . unRun) <$> bFilter <*> bRun
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
