{-# LANGUAGE ScopedTypeVariables #-}
module AdventOfCode.Grid.Dijkstra
    ( dijkstra
    ) where

import qualified AdventOfCode.Grid as G
import           Control.Monad     (guard)
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe        (maybeToList)
import qualified Data.Set          as S

-- | Horrible priority queue
type PrioQueue p a = M.Map p (S.Set a)

empty :: PrioQueue p a
empty = M.empty

push :: (Ord p, Ord a) => p -> a -> PrioQueue p a -> PrioQueue p a
push prio x = M.insertWith S.union prio (S.singleton x)

pop :: (Ord p, Ord a) => PrioQueue p a -> Maybe (p, a, PrioQueue p a)
pop queue0 = case M.minViewWithKey queue0 of
    Nothing -> Nothing
    Just ((prio, xset0), queue1) -> case S.minView xset0 of
        Nothing         -> pop queue1
        Just (x, xset1) -> Just (prio, x, M.insert prio xset1 queue1)

dijkstra
    :: forall a. Ord a
    => (G.Pos -> a -> Bool)  -- ^ Is this cell accessible?
    -> [G.Pos]               -- ^ Starting positions, assumed accessible.
    -> G.Grid a              -- ^ Cells
    -> G.Grid Int            -- ^ Distances
dijkstra accessible starting grid0 = do
    go (L.foldl' (\acc p -> push 0 p acc) empty starting) M.empty
  where
    go :: PrioQueue Int G.Pos -> G.Grid Int -> G.Grid Int
    go queue0 distances0 = case pop queue0 of
        -- Queue empty, we're done
        Nothing -> distances0
        Just (dist, pos , queue1)
            -- Already visited
            | pos `M.member` distances0 -> go queue1 distances0
            -- New visit
            | otherwise ->
                let distances1 = M.insert pos dist distances0
                    neighbours = do
                        p <- G.neighbours pos
                        x <- maybeToList (M.lookup p grid0)
                        guard (accessible p x)
                        return p
                    queue2 = L.foldl'
                        (\acc p -> push (dist + 1) p acc)
                        queue1
                        neighbours in
                go queue2 distances1
