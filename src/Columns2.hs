{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Columns2 (fromGrid, Grid, GridF(..),Grouped(..), Item(..), Single(..), Nested(..), NestedOrSingle(..)) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

data Single a
    = Single Element
    deriving (Functor, Foldable, Traversable)

data Nested a = Nested a
    deriving (Functor, Foldable, Traversable)

data NestedOrSingle a
    = S (Single a)
    | N (Nested a)
    deriving (Functor, Foldable, Traversable)

data Grouped a = Grouped [NestedOrSingle a]
    deriving (Functor, Foldable, Traversable)

data Item a
    = Single' (Single a)
    | Grouped' (Grouped a)
    | Nested' (Nested a)
    deriving (Functor, Foldable, Traversable)

data GridF a = GridF [[Item a]]
    deriving (Functor, Foldable, Traversable)

singleToElem :: Single Element -> UI Element
singleToElem (Single x) = return x

nestedToElem :: Nested Element -> UI Element
nestedToElem (Nested x) = return x

nestedOrSingleToElem :: NestedOrSingle Element -> UI Element
nestedOrSingleToElem (N x) = nestedToElem x
nestedOrSingleToElem (S x) = singleToElem x

groupedToElem :: Grouped Element -> UI Element
groupedToElem (Grouped xs) = do
    items <- mapM nestedOrSingleToElem xs
    UI.div #. "column" # set children items

itemToElem :: Item Element -> UI Element
itemToElem (Single' x) = do
    child <- singleToElem x
    UI.div #. "column" # set children [child]
itemToElem (Grouped' x) = groupedToElem x
itemToElem (Nested' x) = do
    child <- nestedToElem x
    -- gir det her ik en hel container?
    UI.div #. "column" # set children [child]

gridToElem :: GridF Element -> UI Element
gridToElem (GridF xss) = do

    rows <- mapM (\xs -> do
            row <- mapM itemToElem xs
            UI.div #."columns" # set children row
        ) xss

    UI.div #. "container" # set children rows

fromGrid :: Grid -> UI Element
fromGrid = foldFixM $ gridToElem

type Grid = Fix GridF
