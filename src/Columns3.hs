{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Columns3 (test, Row(..), Grid(..), Item(..), toGrid) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid, row, column)

import Data.Fix

import qualified Columns2
import Columns2 (fromGrid)


type Single = UI Element
type Nested = Grid

data NestedOrSingle
    = S' Single
    | N' Nested

data Grouped = Grouped [NestedOrSingle]

data Item
    = S Single
    | M Grouped
    | N Nested

data Row = Row [Item]

data Grid = Grid [UI Row]

elemToSingle :: UI Element -> UI (Columns2.Single Grid)
elemToSingle x = Columns2.Single <$> x

elemToNested :: Grid -> UI (Columns2.Nested Grid)
elemToNested x = return $ Columns2.Nested x

elemToNestedOrSingle :: NestedOrSingle -> UI (Columns2.NestedOrSingle Grid)
elemToNestedOrSingle (N' x) = Columns2.N <$> elemToNested x
elemToNestedOrSingle (S' x) = Columns2.S <$> elemToSingle x

elemToGrouped :: Grouped -> UI (Columns2.Grouped Grid)
elemToGrouped (Grouped xs) = Columns2.Grouped <$> mapM elemToNestedOrSingle xs

elemToItem :: Item -> UI (Columns2.Item Grid)
elemToItem (S x) = Columns2.Single' <$> elemToSingle x
elemToItem (M x) = Columns2.Grouped' <$> elemToGrouped x
elemToItem (N x) = Columns2.Nested' <$> elemToNested x


elemToRow :: Row -> UI (Columns2.Row Grid)
elemToRow (Row xs) = Columns2.Row <$> mapM elemToItem xs

----- LAV ROW HER!
elemToGrid :: Grid -> UI (Columns2.GridF Grid)
elemToGrid (Grid xss) = do
    yss <- sequence xss
    Columns2.GridF <$> mapM elemToRow yss

toGrid :: Grid -> UI Columns2.Grid
toGrid = unfoldFixM elemToGrid


-- FÅ FJERNTET CONTAAINER

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

    let grid = Grid [ return $ Row [S (element contentA), S (element contentB)]
                    , return $ Row [S (element contentC) ]
                    , return $ Row [S (element contentCC), M (Grouped [S' (element contentC1), S' (element contentC2)]) ]
                    --, [S (element contentC), M (Grouped [N' (Grid [[S (element contentA)]]), S' (element contentC2)]) ]
                    -- , [S (element contentCCC), N (Grid [[S (element contentAAA)]])]
                    ]

    myGrid <- fromGrid =<< toGrid grid

    return myGrid
