module Data.Foldable.Extra
    ( minimaBy
    , maximaBy
    , minimumOn
    , maximumOn
    ) where

import qualified Data.Foldable as F
import           Data.Ord      (comparing)

-- | A better `minimumBy`.
minimaBy :: Foldable t => (a -> a -> Ordering) -> t a -> [a]
minimaBy f = F.foldl' step []
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
minimumOn = F.minimumBy . comparing

maximumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
maximumOn = F.maximumBy . comparing
