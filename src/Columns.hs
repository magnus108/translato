{-# LANGUAGE DeriveFunctor #-}
module Columns where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

data Columns = Columns [Column]
data Column = Column [UI Element]


data ColumnsF' a
    = Column' [UI Element]
    | Columns' [a]
        deriving (Functor)

type Columns' = Fix ColumnsF'

col :: Columns'
col = Fix (Column' [UI.div])

lol :: [Columns']
lol = [Fix (Columns' [col, col, Fix (Columns' [col,col])])]

gg :: [Columns'] -> [[UI Element]]
gg = fmap . foldFix $ \x -> case x of
        Column' xs -> [UI.div #. "column" #+ xs]
        Columns' ys -> fmap (\y -> UI.div #. "columns" #+ y) ys



data Single a
    = Single (UI Element)
    | Nested a
    deriving Functor

data Multiple a = Multiple [Single a]
    deriving Functor

data Item a
    = Single' (Single a)
    | Multiple' (Multiple a)
    deriving Functor

data Items a = Items [Item a]
    deriving Functor

type Grid = Fix Items


lola2 :: Grid
lola2 = Fix $ Items
       -- [ Single UI.div, Multiple [Single UI.div, Single UI.div]]
        -- [ Single UI.div, Single UI.div, Single UI.div]
        --[ Multiple [Single (UI.string "bob"), Single (UI.string "bob2")],Single (UI.string "bob") , Nested $ Fix $ Items [ Single (UI.string "bob2")]]
   --     [ Multiple [Multiple [ Single (UI.string "bob"), Single (UI.string "bob2")]],Single (UI.string "bob") , Nested $ Fix $ Items [ Single (UI.string "bob2")]]
        [Multiple' $ Multiple [Nested $ Fix $ Items [Single' $ Single $ UI.string "lols"],Single (UI.string "bob2"), Single (UI.string "bob")], Single' $ Single $ UI.string "baba"]



grid5 :: Single (UI Element) -> UI Element
grid5 x = case x of
            Single x -> x
            Nested z -> z


grid3 :: Item (UI Element) -> UI Element
grid3 x = case x of
            (Single' y) -> UI.div #. "column" #+ [grid5 y]
            (Multiple' (Multiple ys)) -> UI.div #. "column" #+ (fmap grid5 ys)

makeIt :: Items (UI Element) -> UI Element
makeIt (Items xs) = UI.div #. "columns" #+ (fmap (\x -> grid3 x) xs)

grid2 :: Grid -> UI Element
grid2 = foldFix $ makeIt


grid :: [Columns] -> [UI Element]
grid columns = (\(Columns cols) -> UI.div #. "columns" #+ ((\(Column col) -> UI.div #. "column" #+ col) <$> cols)) <$> columns
