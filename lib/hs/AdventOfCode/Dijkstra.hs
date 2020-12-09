module AdventOfCode.Dijkstra
    ( Bfs (..)
    , bfs

    , Dijkstra (..)
    , dijkstra
    ) where

import qualified AdventOfCode.PriorityQueue as PQ
import           Control.Monad              (guard)
import qualified Data.List                  as L
import qualified Data.Map                   as Map

data Bfs v = Bfs
    { bfsDistances :: Map.Map v [v]
    , bfsGoal      :: Maybe (v, [v])
    }

bfs :: Ord v
    => (v -> [v])   -- ^ Neighbours
    -> (v -> Bool)  -- ^ Is this a goal?
    -> v            -- ^ Start
    -> Bfs v
bfs neighbours goal start = go Map.empty (Map.singleton start [start])
  where
    go visited fringe
        | Map.null fringe = Bfs visited Nothing
        | g : _ <- goals  = Bfs visited' (Just g)
        | otherwise       = go visited' fringe'
      where
        goals    = filter (goal . fst) $ Map.toList fringe
        visited' = Map.unionWith const visited fringe
        fringe'  = Map.fromList $ do
            (n, path) <- Map.toList fringe
            nb <- neighbours n
            guard . not $ nb `Map.member` visited'
            pure (nb, nb : path)

data Dijkstra v = Dijkstra
    { dijkstraDistances :: Map.Map v (Int, [v])
    , dijkstraGoal      :: Maybe (v, Int, [v])
    }

dijkstra
    :: Ord v
    => (v -> [(Int, v)])     -- ^ Neighbours
    -> (v -> Bool)           -- ^ Is this a goal?
    -> v                     -- ^ Start
    -> Dijkstra v
dijkstra neighbours goal start =
    go (PQ.singleton 0 (start, [start])) Map.empty
  where
    go fringe0 dists0 = case PQ.pop fringe0 of
        Nothing -> Dijkstra dists0 Nothing
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

                if goal current
                    then Dijkstra dists1 $ Just (current, cdist, path)
                    else go fringe2 dists1
