{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Lib (
    -- * Widgets
    -- ** Input widgets
    TextEntry, entry, userText,
    -- ** ListBox
    listBox
    ) where

import Model

import Control.Monad (void, when)
import qualified Data.Map                          as Map

import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Events     as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

{-----------------------------------------------------------------------------
    Input widgets
------------------------------------------------------------------------------}
-- | A single-line text entry.
data TextEntry = TextEntry
    { _elementTE :: Element
    , _userTE    :: Tidings String
    }

instance Widget TextEntry where getElement = _elementTE

-- | User changes to the text value.
userText :: TextEntry -> Tidings String
userText = _userTE

-- | Create a single-line text entry.
entry
    :: Behavior String  -- ^ Display value when the element does not have focus.
    -> UI TextEntry
entry bValue = do -- single text entry
    input <- UI.input

    bEditing <- stepper False $ and <$>
        unions [True <$ UI.focus input, False <$ UI.blur input]

    window <- askWindow
    liftIOLater $ onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    let _elementTE = input
        _userTE    = tidings bValue $ UI.valueChange input
    return TextEntry {..}

------------------------------------------------------------------------------
listBox :: Behavior Run -> UI Element
listBox bItems = do
    list <- UI.div

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
