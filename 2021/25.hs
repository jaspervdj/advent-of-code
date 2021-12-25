{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Main where

import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.V2           (V2 (..))

data Tile = South | East | Empty deriving (Eq)
type Grid = G.Grid Tile

north, south, east, west :: Grid -> G.Pos -> G.Pos
north g (V2 x y) = V2 x (if y <= 0 then G.gridHeight g - 1 else y - 1)
south g (V2 x y) = V2 x (if y + 1 >= G.gridHeight g then 0 else y + 1)
east  g (V2 x y) = V2 (if x + 1 >= G.gridWidth g then 0 else x + 1) y
west  g (V2 x y) = V2 (if x <= 0 then G.gridWidth g - 1 else x - 1) y

step :: Grid -> Grid
step = stepSouth . stepEast
  where
    stepEast g = G.mapWithKey (\pos tile -> case tile of
        East  | g G.! east g pos == Empty -> Empty
        Empty | g G.! west g pos == East  -> East
        _                                 -> tile) g

    stepSouth g = G.mapWithKey (\pos tile -> case tile of
        South | g G.! south g pos == Empty -> Empty
        Empty | g G.! north g pos == South -> South
        _                                  -> tile) g

parseInput :: String -> Either String Grid
parseInput input = G.fromString input >>= traverse (\case
    '.' -> Right Empty
    '>' -> Right East
    'v' -> Right South
    c   -> Left $ "Unknown character: " ++ show c)

gridToString :: Grid -> String
gridToString = G.toString . fmap (\case
    Empty -> '.'
    East  -> '>'
    South -> 'v')

solve :: Grid -> (Int, Grid)
solve = go 1
  where
    go !acc grid =
        let grid' = step grid in
        if grid == grid' then (acc, grid) else go (acc + 1) grid'

main :: IO ()
main = pureMain $ \input -> do
    grid0 <- parseInput input
    let (steps, grid) = solve grid0
    pure (pure steps, pure (gridToString grid))
