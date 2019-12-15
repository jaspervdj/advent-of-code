module AdventOfCode.AStar
    ( astar
    ) where

import qualified AdventOfCode.PriorityQueue as PQ
import           Control.Monad              (guard)
import qualified Data.List                  as L
import qualified Data.Map                   as Map

astar
    :: Ord v
    => (v -> [(Int, v)])  -- ^ Neighbours
    -> (v -> Int)         -- ^ Estimate to goal
    -> v                  -- ^ Start
    -> v                  -- ^ Goal
    -> Maybe (Int, [v])   -- ^ Distance, path.
astar neighbours estimate start goal =
    go (PQ.singleton (estimate start) (start, [])) (Map.singleton start 0)
  where
    go fringe0 gscore = case PQ.pop fringe0 of
        Nothing -> Nothing
        Just (_, (current, path), fringe1)
            | current == goal -> Just (cdist, reverse path)
            | otherwise ->
                let interesting = do
                        (d, neighbour) <- neighbours current
                        let tentative = cdist + d
                        guard $ case Map.lookup neighbour gscore of
                            Nothing   -> True
                            Just dist -> tentative < dist
                        pure (tentative, neighbour) in
            go
                (L.foldl'
                    (\acc (d, v) -> PQ.push (d + estimate v) (v, v : path) acc)
                    fringe1 interesting)
                (L.foldl'
                    (\acc (d, v) -> Map.insert v d acc)
                    gscore interesting)
          where
            cdist = case Map.lookup current gscore of
                Nothing -> error "astar: missing gscore"
                Just s  -> s
