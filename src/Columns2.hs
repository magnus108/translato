{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Columns2 (fromGrid2, gridToElem2, Grid, Row(..), GridF(..),Grouped(..), Item(..), Single(..), Nested(..), NestedOrSingle(..)) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix


data Single a
    = Single (UI Element)
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


data GridF m a = GridF [m (Row a)]
    deriving (Functor, Foldable, Traversable)


type Grid = Fix (GridF UI)


singleToElem2 :: Single [UI Element] -> [UI Element]
singleToElem2 (Single x) = [x]


nestedToElem2 :: Nested [UI Element] -> [UI Element]
nestedToElem2 (Nested x) = x


nestedOrSingleToElem2 :: NestedOrSingle [UI Element] -> [UI Element]
nestedOrSingleToElem2 (N x) = nestedToElem2 x
nestedOrSingleToElem2 (S x) = singleToElem2 x


groupedToElem2 :: Grouped [UI Element] -> UI Element
groupedToElem2 (Grouped xs) = do
    let items = fmap nestedOrSingleToElem2 xs
    UI.div #. "column" #+ (concat items)


itemToElem2 :: Item [UI Element] -> UI Element
itemToElem2 (Single' x) = do
    let children' = singleToElem2 x
    UI.div #. "column" #+ children'
itemToElem2 (Grouped' x) = groupedToElem2 x
itemToElem2 (Nested' x) = do
    let children' = nestedToElem2 x
    UI.div #. "column" #+ children'


rowToElem2 :: UI (Row [UI Element]) -> UI Element
rowToElem2 xs = do
        row <- (\(Row y) -> fmap itemToElem2 y) <$> xs
        UI.div #."columns" #+ row


gridToElem2 :: GridF UI [UI Element] -> [UI Element]
gridToElem2 (GridF xss) = fmap rowToElem2 xss


fromGrid2 :: Grid -> [UI Element]
fromGrid2  grid = foldFix gridToElem2 grid


