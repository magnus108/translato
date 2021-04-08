{-# LANGUAGE DeriveFunctor #-}
module Columns where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

data Single a
    = Single (UI Element)
    deriving Functor

data Nested a = Nested a
    deriving Functor

data NestedOrSingle a
    = S (Single a)
    | N (Nested a)
    deriving Functor

data Multiple a = Multiple [NestedOrSingle a]
    deriving Functor

data Item a
    = Single' (Single a)
    | Multiple' (Multiple a)
    | Nested' (Nested a)
    deriving Functor

data Items a = Items [[Item a]]
    deriving Functor

type Grid = Fix Items


helper2 :: NestedOrSingle [UI Element] -> UI Element
helper2 x = case x of
            S (Single x) -> x
            N (Nested z) -> UI.div #+ z


helper1 :: Item [UI Element] -> UI Element
helper1 x = case x of
            (Single' (Single y)) -> UI.div #. "column" #+ [y]
            (Multiple' (Multiple ys)) -> UI.div #. "column" #+ (fmap helper2 ys)
            (Nested' (Nested ys)) -> UI.div #. "column" #+ ys


helper :: Items [UI Element] -> [UI Element]
helper (Items xss) = fmap (\xs -> UI.div #. "columns" #+ fmap helper1 xs) xss

grid :: Grid -> [UI Element]
grid = foldFix $ helper

    {-
testGrid2 :: Grid
testGrid2 = t [[]]


testGrid4 :: Item Grid
testGrid4 = g [[]]

testGrid3 :: Grid
testGrid3 = t [[toItem testGrid2, g [[]], testGrid4]]

--testGrid5 :: Grid
--testGrid5 = unItem testGrid4

testGrid :: Grid
testGrid = t
        [ [s $ UI.string "lols"]
        , [s $ UI.string "lols", s $ UI.string "lols2"]
        , [g [[s $ UI.string "lols"]]]
        , [g [[s $ UI.string "lols3"], [s $ UI.string "lols"]]]
        , [m [s' $ UI.string "lols"]]
        , [m [s' $ UI.string "lols", s' $ UI.string "lols"]]
        , [m [s' $ UI.string "lols"], s $ UI.string "lols"]
        , [m [g' [[s $ UI.string "lols"]], s' $ UI.string "lols"]]
        ]
        -}

-- Top
t :: [[Item Grid]] -> Grid
t = Fix . Items

-- Lift
toItem :: Grid -> Item Grid
toItem = Nested' . Nested

-- Helpers
s'' :: UI Element -> Single Grid
s'' = Single

g'' :: [[Item Grid]] -> Nested Grid
g'' = Nested . t

-- Items
s :: UI Element ->  Item Grid
s = Single' . s''

m :: [NestedOrSingle Grid] -> Item Grid
m = Multiple' . Multiple

g :: [[Item Grid]] ->  Item Grid
g = Nested' . g''

-- Multiple
s' :: UI Element -> NestedOrSingle Grid
s' = S . s''

g' :: [[Item Grid]] -> NestedOrSingle Grid
g' = N . g''
