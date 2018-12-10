{-# LANGUAGE DeriveFunctor #-}
module AdventOfCode.V2.Line
    ( Line (..)
    , intersection
    ) where

import           AdventOfCode.V2

data Line a = Line
    { l1 :: !(V2 a)
    , l2 :: !(V2 a)
    } deriving (Functor, Show)

intersection :: (Eq a, Fractional a, Num a) => Line a -> Line a -> Maybe (V2 a)
intersection
        (Line p1@(V2 x1 y1) p2@(V2 x2 y2))
        (Line p3@(V2 x3 y3) p4@(V2 x4 y4))
    | denominator == 0 = Nothing
    | otherwise        = Just $ p1 .+. (p2 .-. p1) .* t
  where
    nominator   = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    t           = nominator / denominator
