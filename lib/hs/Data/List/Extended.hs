module Data.List.Extended
    ( module Data.List
    , select
    , (!!?)
    , minimaBy
    , maximaBy
    , minimumOn
    , maximumOn
    , lexicographicSuccessor
    ) where

import           Control.Monad (guard)
import           Data.List
import           Data.Maybe    (listToMaybe)
import           Data.Ord      (comparing)

select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

-- | Like '!!', but returns a 'Maybe'.
(!!?) :: [a] -> Int -> Maybe a
list !!? idx = guard (idx >= 0) >> listToMaybe (drop idx list)

-- | A better `minimumBy`.
minimaBy :: Foldable t => (a -> a -> Ordering) -> t a -> [a]
minimaBy f = foldl' step []
  where
    step []       x = [x]
    step (y : ys) x = case f x y of
        EQ -> x : y : ys
        LT -> [x]
        GT -> y : ys

maximaBy :: (Ord a, Foldable t) => (a -> a -> Ordering) -> t a -> [a]
maximaBy f = minimaBy (\x y -> down (f x y))
  where
    down LT = GT
    down EQ = EQ
    down GT = LT

minimumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
minimumOn = minimumBy . comparing

maximumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
maximumOn = maximumBy . comparing

lexicographicSuccessor :: (Bounded a, Enum a, Eq a) => [a] -> [a]
lexicographicSuccessor =
    \xs -> let (ys, carry) = go xs in if carry then minBound : ys else ys
  where
    go [] = ([], True)
    go (x : xs) =
        let (ys, carry) = go xs
            y           = if x == maxBound then minBound else succ x in
        if carry then (y : ys, x == maxBound) else (x : ys, False)
