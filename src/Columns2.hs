{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Columns2 (fromGrid, fromGrid2, Grid, Row(..), GridF(..),Grouped(..), Item(..), Single(..), Nested(..), NestedOrSingle(..)) where

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

data Row a = Row [Item a]
    deriving (Functor, Foldable, Traversable)

data GridF a = GridF [Row a]
    deriving (Functor, Foldable, Traversable)

singleToElem :: Single [Element] -> UI [Element]
singleToElem (Single x) = return [x]

singleToElem2 :: Single [UI Element] -> [UI Element]
singleToElem2 (Single x) = [return x]

nestedToElem :: Nested [Element] -> UI [Element]
nestedToElem (Nested x) = return x

nestedToElem2 :: Nested [UI Element] -> [UI Element]
nestedToElem2 (Nested x) = x

nestedOrSingleToElem :: NestedOrSingle [Element] -> UI [Element]
nestedOrSingleToElem (N x) = nestedToElem x
nestedOrSingleToElem (S x) = singleToElem x

nestedOrSingleToElem2 :: NestedOrSingle [UI Element] -> [UI Element]
nestedOrSingleToElem2 (N x) = nestedToElem2 x
nestedOrSingleToElem2 (S x) = singleToElem2 x

groupedToElem2 :: Grouped [UI Element] -> UI Element
groupedToElem2 (Grouped xs) = do
    let items = fmap nestedOrSingleToElem2 xs
    UI.div #. "column" #+ (concat items)

groupedToElem :: Grouped [Element] -> UI Element
groupedToElem (Grouped xs) = do
    items <- mapM nestedOrSingleToElem xs
    UI.div #. "column" # set children (concat items)

itemToElem :: Item [Element] -> UI Element
itemToElem (Single' x) = do
    children' <- singleToElem x
    UI.div #. "column" # set children children'
itemToElem (Grouped' x) = groupedToElem x
itemToElem (Nested' x) = do
    children' <- nestedToElem x
    UI.div #. "column" # set children children'

itemToElem2 :: Item [UI Element] -> UI Element
itemToElem2 (Single' x) = do
    let children' = singleToElem2 x
    UI.div #. "column" #+ children'
itemToElem2 (Grouped' x) = groupedToElem2 x
itemToElem2 (Nested' x) = do
    let children' = nestedToElem2 x
    UI.div #. "column" #+ children'


rowToElem :: Row [Element] -> UI Element
rowToElem (Row xs) = do
        row <- mapM itemToElem xs
        UI.div #."columns" # set children row

rowToElem2 :: Row [UI Element] -> UI Element
rowToElem2 (Row xs) = do
        let row = fmap itemToElem2 xs
        UI.div #."columns" #+ row

gridToElem :: GridF [Element] -> UI [Element]
gridToElem (GridF xss) = do
    rows <- mapM rowToElem xss
    return rows

gridToElem2 :: GridF [UI Element] -> [UI Element]
gridToElem2 (GridF xss) = fmap rowToElem2 xss


fromGrid :: Grid -> UI Element
fromGrid grid = do
    children' <- foldFixM gridToElem grid
    UI.div # set children children'

fromGrid2 :: Grid -> [UI Element]
fromGrid2  grid = foldFix gridToElem2 grid


type Grid = Fix GridF
