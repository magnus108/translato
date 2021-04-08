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

grid :: [Columns] -> [UI Element]
grid columns = (\(Columns cols) -> UI.div #. "columns" #+ ((\(Column col) -> UI.div #. "column" #+ col) <$> cols)) <$> columns



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

data Items a = Items [[Item a]]
    deriving Functor

type Grid = Fix Items


lola2 :: Grid
lola2 = Fix $ Items
       -- [ Single UI.div, Multiple [Single UI.div, Single UI.div]]
        -- [ Single UI.div, Single UI.div, Single UI.div]
        --[ Multiple [Single (UI.string "bob"), Single (UI.string "bob2")],Single (UI.string "bob") , Nested $ Fix $ Items [ Single (UI.string "bob2")]]
   --     [ Multiple [Multiple [ Single (UI.string "bob"), Single (UI.string "bob2")]],Single (UI.string "bob") , Nested $ Fix $ Items [ Single (UI.string "bob2")]]
        --[[Multiple' $ Multiple [Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"]],Single (UI.string "bob2"), Single (UI.string "bob")], Single' $ Single $ UI.string "baba"]]
        [ [Single' $ Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"]]]
        , [Single' $ Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"]]]
        , [Single' $ Single $ UI.string "lols", Single' $ Single $ UI.string "lols2"]
        , [Single' $ Single $ UI.string "lols", Multiple' $ Multiple [Single $ UI.string "lols", Single $ UI.string "lols2"]]
        , [Single' $ Single $ UI.string "lols", Multiple' $ Multiple [Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"], [Single' $ Single $ UI.string "lols"],[Single' $ Single $ UI.string "lols"]],  Single $ UI.string "lols", Single $ UI.string "lols2"]]
        ]



grid8 :: Single [UI Element] -> UI Element
grid8 x = case x of
            Single x -> UI.div #. "column" #+ [x]
            Nested z -> UI.div #. "column" #+ z


grid9 :: Single [UI Element] -> UI Element
grid9 x = case x of
            Single x -> x
            Nested z -> UI.div #. "columns" #+ [UI.div #. "column" #+ z]


grid7 :: Item [UI Element] -> UI Element
grid7  x = case x of
            (Single' y) -> grid8 y
            (Multiple' (Multiple ys)) -> UI.div #. "column" #+ fmap grid9 ys


makeIt :: Items [UI Element] -> [UI Element]
makeIt (Items xss) = fmap (\xs -> UI.div #. "columns" #+ fmap grid7 xs) xss 

grid2 :: Grid -> [UI Element]
grid2 = foldFix $ makeIt


