{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Columns3 (Grouped(..), NestedOrSingle(..), Row(..), Grid(..), Item(..), toGrid, construct) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

import qualified Columns2
import Columns2 (fromGrid2, gridToElem2)


type SuperSingle = UI Element
type Single = UI Element
type Nested = Grid

data NestedOrSingle
    = S' Single
    | N' Nested

data Grouped = Grouped [NestedOrSingle]

data Item
    = S Single
    | SS SuperSingle
    | M Grouped
    | N Nested

data Row = Row [Item]

data Grid = Grid [UI Row]

elemToSingle :: UI Element -> Columns2.Single Grid
elemToSingle x = Columns2.Single x

elemToSuperSingle :: UI Element -> Columns2.SuperSingle Grid
elemToSuperSingle x = Columns2.SuperSingle x

elemToNested :: Grid -> Columns2.Nested Grid
elemToNested x = Columns2.Nested x

elemToNestedOrSingle :: NestedOrSingle -> Columns2.NestedOrSingle Grid
elemToNestedOrSingle (N' x) = Columns2.N $ elemToNested x
elemToNestedOrSingle (S' x) = Columns2.S $ elemToSingle x

elemToGrouped :: Grouped -> Columns2.Grouped Grid
elemToGrouped (Grouped xs) = Columns2.Grouped $ fmap elemToNestedOrSingle xs

elemToItem :: Item -> Columns2.Item Grid
elemToItem (S x) = Columns2.Single' $ elemToSingle x
elemToItem (SS x) = Columns2.SuperSingle' $ elemToSuperSingle x
elemToItem (M x) = Columns2.Grouped' $ elemToGrouped x
elemToItem (N x) = Columns2.Nested' $ elemToNested x

elemToRow :: UI Row -> UI (Columns2.Row Grid)
elemToRow ys = (\(Row y) -> Columns2.Row $ fmap elemToItem y) <$> ys

elemToGrid :: Grid -> Columns2.GridF UI Grid
elemToGrid (Grid xss) = Columns2.GridF $ fmap elemToRow xss

toGrid :: Grid -> Columns2.Grid
toGrid = unfoldFix elemToGrid

construct :: Grid -> [UI Element]
construct = refold gridToElem2 elemToGrid

    {-
test :: UI Element
test = do
    contentA <- UI.div # set text "a"
    contentAAA <- UI.div # set text "aaaaaa"
    contentA1 <- UI.div # set text "a1"
    contentB <- UI.div # set text "b"
    contentC <- UI.div # set text "c"
    contentCC <- UI.div # set text "c2"
    contentCCC <- UI.div # set text "c3"
    contentC1 <- UI.div # set text "c1"
    contentC2 <- UI.div # set text "c2"
    return contentC1

    let grid = Grid [ return $ Row [S (element contentA), S (element contentB)]
                    , return $ Row [S (element contentC) ]
                    , return $ Row [S (element contentCC), M (Grouped [S' (element contentC1), S' (element contentC2)]) ]
                    , return $ Row [S (element contentC), M (Grouped [N' (Grid [return $ Row [S (element contentA)]]), S' (element contentC2)]) ]
                    , return $ Row [S (element contentCCC), N (Grid [return $ Row [S (element contentAAA)]])]
                    ]

    let myGrid = fromGrid2 (toGrid grid)

    UI.div #+ myGrid
    -}
