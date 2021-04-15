{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Columns3 (test) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

import qualified Columns2
import Columns2 (fromGrid)


type Single = Element
type Nested = Grid

data NestedOrSingle
    = S' Single
    | N' Nested

data Grouped = Grouped [NestedOrSingle]

data Item
    = S Single
    | M Grouped
    | N Nested

data Grid = Grid [[Item]]

elemToSingle :: Element -> Columns2.Single Grid
elemToSingle x = Columns2.Single x

elemToNested :: Grid -> Columns2.Nested Grid
elemToNested x = Columns2.Nested x

elemToNestedOrSingle :: NestedOrSingle -> Columns2.NestedOrSingle Grid
elemToNestedOrSingle (N' x) = Columns2.N $ elemToNested x
elemToNestedOrSingle (S' x) = Columns2.S $ elemToSingle x

elemToGrouped :: Grouped -> Columns2.Grouped Grid
elemToGrouped (Grouped xs) = Columns2.Grouped $ fmap elemToNestedOrSingle xs

elemToItem :: Item -> Columns2.Item Grid
elemToItem (S x) = Columns2.Single' $ elemToSingle x
elemToItem (M x) = Columns2.Grouped' $ elemToGrouped x
elemToItem (N x) = Columns2.Nested' $ elemToNested x

elemToGrid :: Grid -> Columns2.GridF Grid
elemToGrid (Grid xss) = Columns2.GridF $ fmap (fmap elemToItem) xss

toGrid :: Grid -> Columns2.Grid
toGrid = unfoldFix elemToGrid


test :: UI Element
test = do
    contentA <- UI.div # set text "a"
    contentA1 <- UI.div # set text "a1"
    contentB <- UI.div # set text "b"
    contentC <- UI.div # set text "c"
    contentC1 <- UI.div # set text "c1"
    contentC2 <- UI.div # set text "c2"

    let grid = Grid [[S contentA, S contentB]
               , [S contentC ]
               , [S contentC, M (Grouped [S' contentC1, S' contentC2]) ]
               , [S contentC, M (Grouped [N' (Grid [[S contentA]]), S' contentC2]) ]
               , [N (Grid [[S contentA]])]
               ]

    let grid2 = [[S contentA1, N grid]]
    elem <- fromGrid (toGrid (Grid grid2))
    return elem
