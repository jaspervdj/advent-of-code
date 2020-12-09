module Main where

import           Control.Monad (guard)
import           Data.Array    (Array, Ix)
import qualified Data.Array    as Arr
import           Data.Monoid   (Sum (..))

invalids :: Int -> Array Int Int -> [Int]
invalids windowSize arr = go (lo + windowSize)
  where
    (lo, hi) = Arr.bounds arr
    go i
        | i > hi     = []
        | null atoms = (arr Arr.! i) : go (i + 1)
        | otherwise  = go (i + 1)
      where
        atoms = do
            j <- [i - windowSize .. i - 2]
            k <- [j + 1 .. i - 1]
            guard $ (arr Arr.! j) + (arr Arr.! k) == (arr Arr.! i)
            pure (j, k)

mcontiguous :: (Enum i, Ix i, Semigroup a) => Array i a -> Array (i, i) a
mcontiguous arr = out
  where
    (lo, hi) = Arr.bounds arr
    out = Arr.array ((lo, lo), (hi, hi)) $ do
        i <- [lo .. hi]
        j <- [lo .. hi]
        let x = case compare i j of
                EQ -> arr Arr.! i
                GT -> out Arr.! (j, i)
                LT -> out Arr.! (i, pred j) <> arr Arr.! j
        pure ((i, j), x)

main :: IO ()
main = do
    numbers <- map read . lines <$> getContents
    let arr = Arr.listArray (0, length numbers - 1) numbers
        part1 = head $ invalids 25 arr
        sums = mcontiguous $ fmap Sum arr
        part2 = head $ do
            ((i, j), x) <- Arr.assocs sums
            guard $ x == Sum part1 && j > i
            let ys = [arr Arr.! k | k <- [i .. j]]
            pure (minimum ys + maximum ys)
    print part1
    print part2
