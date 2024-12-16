module Main where

import qualified AdventOfCode.Dijkstra     as Dijkstra
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.V2           (V2 (..))
import           Data.Char                 (digitToInt, isDigit)
import           Data.Maybe                (mapMaybe, maybeToList)
import qualified Data.Set                  as S
import           Data.Traversable          (for)

solve :: G.Grid Int -> Int
solve grid = sum $
    mapMaybe (`G.lookup` grid) (drop 1 $ Dijkstra.backtrack end dijkstra)
  where
    start    = V2 0 0
    end      = V2 (G.gridWidth grid - 1) (G.gridHeight grid - 1)
    dijkstra = Dijkstra.dijkstra Dijkstra.Options
        { Dijkstra.neighbours = \pos -> do
            n <- G.neighbours pos
            v <- maybeToList $ G.lookup n grid
            pure (v, n)
        , Dijkstra.find = Dijkstra.FindOne (== end)
        , Dijkstra.start = S.singleton start
        }

extend :: Int -> G.Grid Int -> G.Grid Int
extend n grid = G.generate (n * gw) (n * gh) $ \(V2 x y) ->
    let (gx, x') = divMod x gw
        (gy, y') = divMod y gh
        val = G.index (V2 x' y') grid in
    (val + gx + gy - 1) `mod` 9 + 1
  where
    gw = G.gridWidth grid
    gh = G.gridHeight grid

main :: IO ()
main = pureMain $ \input -> do
    cgrid <- G.fromString input
    grid <- for cgrid $ \c ->
        if isDigit c then Right (digitToInt c) else Left ("Bad digit: " ++ [c])
    let part1 = solve grid
        part2 = solve $ extend 5 grid
    pure (pure part1, pure part2)
