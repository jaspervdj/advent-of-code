module AdventOfCode.Dijkstra
    ( dijkstra
    ) where

import qualified AdventOfCode.PriorityQueue as PQ
import           Control.Monad              (guard)
import qualified Data.List                  as L
import qualified Data.Map                   as Map

dijkstra
    :: Ord v
    => (v -> [(Int, v)])     -- ^ Neighbours
    -> (v -> Bool)           -- ^ Is this a goal?
    -> v                     -- ^ Start
    -> Map.Map v (Int, [v])  -- ^ Distance, path.
dijkstra neighbours goal start =
    go (PQ.singleton 0 (start, [start])) Map.empty
  where
    go fringe0 dists0 = case PQ.pop fringe0 of
        Nothing -> dists0
        Just (cdist, (current, path), fringe1)
            | current `Map.member` dists0 -> go fringe1 dists0
            | otherwise ->
                let interesting = do
                        (d, neighbour) <- neighbours current
                        guard $ not $ neighbour `Map.member` dists0
                        pure (cdist + d, neighbour)

                    dists1 = Map.insert current (cdist, path) dists0

                    fringe2 = L.foldl'
                        (\acc (d, v) -> PQ.push d (v, v : path) acc)
                        fringe1 interesting in

                if goal current then dists1 else go fringe2 dists1
