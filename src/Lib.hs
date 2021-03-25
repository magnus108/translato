{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Lib (
    TextEntry, entry, userText,
    listBox
           ,brah
           , kv
           , lookup
    ) where

import Model
import Data.Generics.Labels
import           Options.Generic

import qualified Data.Map.Strict               as M
import           Control.Comonad
import Control.Comonad.Store
import qualified Control.Comonad.Store.Class   as Store

import           Control.Lens                   ( (^.), (.~))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
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

------------------------------------------------------------------------------

kv :: (ComonadStore Status w) => w String -> (String, String)
kv w = (position (Store.pos w), extract w)

lookup :: String -> Run -> String
lookup key = Store.peeks (Lens.set #position key) . unRun

brah :: (ComonadStore Status w) => String -> w String -> w String
brah translation run =
    let status = pos run
        position = status ^. #position
        translations = status ^. #translations . #unTranslations
        translations' = Translations $ M.insert position translation translations
        status' = status & #translations .~ translations'
    in
        Store.seeks (const status') run


listBox :: Behavior Run ->  Behavior (String -> Bool) -> UI Element
listBox bRun bFilter = do
    list <- UI.div

    let bItems = (\p -> filter (p . fst) . Store.experiment (\status' -> let elems = (M.keys (status' ^. #translations . #unTranslations)) in fmap (\e -> status' & #position .~ e) elems) . extend kv . unRun) <$> bFilter <*> bRun
    let bDisplay = pure $ \(x,y) -> UI.div #+ [UI.string x, UI.string y]
    element list # sink items (fmap <$> bDisplay <*> bItems)

    return list
    {-
    -- animate output items
    element list # sink items (map <$> bdisplay <*> bitems)

    -- animate output selection
    let bindices :: Behavior (Map.Map a Int)
        bindices = (Map.fromList . flip zip [0..]) <$> bitems
        bindex   = lookupIndex <$> bindices <*> bsel

        lookupIndex indices Nothing    = Nothing
        lookupIndex indices (Just sel) = Map.lookup sel indices

    element list # sink UI.selection bindex

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 :: Behavior (Map.Map Int a)
        bindices2 = Map.fromList . zip [0..] <$> bitems

        _selectionLB = tidings bsel $
            lookupIndex <$> bindices2 <@> UI.selectionChange list
        _elementLB   = list

    return ListBox {..}

items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.option #+ [i]) i
    -}
-------------------------------------------------------------------------------
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
