{-# LANGUAGE DeriveFunctor #-}
module Columns where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

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


testGrid :: Grid
testGrid = Fix $ Items
        [ [Single' $ Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"]]]
        , [Single' $ Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"]]]
        , [Single' $ Single $ UI.string "lols", Single' $ Single $ UI.string "lols2"]
        , [Single' $ Single $ UI.string "lols", Multiple' $ Multiple [Single $ UI.string "lols", Single $ UI.string "lols2"]]
        , [Single' $ Single $ UI.string "lols", Multiple' $ Multiple [Nested $ Fix $ Items [[Single' $ Single $ UI.string "lols"], [Single' $ Single $ UI.string "lols"],[Single' $ Single $ UI.string "lols"]],  Single $ UI.string "lols", Single $ UI.string "lols2"]]
        ]



helper3 :: Single [UI Element] -> UI Element
helper3 x = case x of
            Single x -> UI.div #. "column" #+ [x]
            Nested z -> UI.div #. "column" #+ z


helper2 :: Single [UI Element] -> UI Element
helper2 x = case x of
            Single x -> x
            Nested z -> UI.div #. "columns" #+ [UI.div #. "column" #+ z]


helper1 :: Item [UI Element] -> UI Element
helper1  x = case x of
            (Single' y) -> helper3 y
            (Multiple' (Multiple ys)) -> UI.div #. "column" #+ fmap helper2 ys


helper :: Items [UI Element] -> [UI Element]
helper (Items xss) = fmap (\xs -> UI.div #. "columns" #+ fmap helper1 xs) xss

grid :: Grid -> [UI Element]
grid = foldFix $ helper


