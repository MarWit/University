{-|
Module: MiodoweSolver
Description: Module which exports Miodowe and it's solver
Copyright: Marcin Witkowski

Doesn't work. Note above cellsXYZ function
|-}

module MiodoweSolver (Miodowe(..), Cells, solver) where

import Control.Monad
import Data.Maybe

type Cell = (Int, Int)
type Cells = [Cell]
type CellXYZ = (Int, Int, Int)
type CellsXYZ = [CellXYZ]
data Miodowe = Miodowe {
    size :: Int,
    islands_num :: Int,
    island_size :: Int,
    cells :: Cells
}

-- Prawdopodobnie popsułem zamiane koordynatów, przez co nie działa...
-- Idea tutaj była taka, że można używać koordynatów gdzie
-- (x + y + z) == 0, \/x  |x| <= size
-- wtedy nie trzeba sie bawić w żadne skalowanie
cellsXYZ :: Cells -> CellsXYZ
cellsXYZ = map (\(c, r) ->
    let x = c - (r - (r `mod` 2)) `div` 2 in
    (x, -x-r, r))

cellsXY :: CellsXYZ -> Cells
cellsXY = map (\(x, _, z) -> (
    x + (z - (z `mod` 2)) `div` 2,
    z))

solver :: MonadPlus m => Miodowe -> m Cells
solver m = do
    let holes = cellsXYZ . cells $ m
    let possible_islands =
            [(x, y, z) | x <- [-s..s],
                         y <- [-s..s],
                         z <- [-s..s],
                         (x+y+z) == 0,
                         (x,y,z) `notElem` holes]
    found <- consider m possible_islands holes []
    return $ cellsXY found
        where s = size m

consider :: MonadPlus m => Miodowe -> CellsXYZ -> CellsXYZ -> CellsXYZ -> m CellsXYZ
consider m (c:cs) holes islands =
    consider m cs (c:holes) islands `mplus` consider m cs holes (c:islands)
consider m [] holes islands = do
    let clusters = build_clusters [] islands
    guard $ length clusters == num
    guard $ all ((==size) . length) clusters
    return holes
        where num = islands_num m
              size = island_size m

distance (x, y, z) (u, v, w) =
    abs (x - u) + abs (y - v) + abs (z - w)

build_clusters clusters (i:is) = build_clusters (insert i clusters) is
    where insert e (c:cs)
              | any ((== 1) . (distance e)) c = (e : c) : cs
              | otherwise = c : insert e cs
          insert e [] = [[e]]
build_clusters (c:clusters) [] =
    case connect [] c clusters of
        Just new_clusters -> build_clusters new_clusters []
        Nothing -> c : build_clusters clusters []
    where connect _ _ [] = Nothing
          connect acc island (i:is)
            | or [distance a b == 1 | a <- island, b <- i] = Just ((i ++ island) : (acc ++ is))
            | otherwise = connect (i:acc) island is
build_clusters [] [] = []

jakies = Miodowe {
    size = 4,
    islands_num = 6,
    island_size = 6,
    cells = [(-2, 4), (-1, 3), (-6, 2), ( 6, 2), ( 2, 2), ( 3, 1), (-4, 0), ( 0, 0),( 6, 0), (-5,-1), (-1,-1), ( 4,-2), ( 6,-2), (-5,-3), ( 0,-4)]
}
