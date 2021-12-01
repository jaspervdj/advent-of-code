{-# LANGUAGE BangPatterns #-}
module Main where

import           AdventOfCode.Main
import           Data.List         (foldl')

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n things =
    let window = take n things in
    if length window < n then [] else window : slidingWindow n (drop 1 things)

numIncreases :: Ord a => [a] -> Int
numIncreases = maybe 0 snd . foldl'
    (\mbPrev num -> case mbPrev of
        Nothing        -> Just (num, 0 :: Int)
        Just (n, !inc) -> Just (num, inc + if num > n then 1 else 0))
    Nothing

main :: IO ()
main = simpleMain $ \input ->
    let numbers = map read $ lines input :: [Int] in
    (numIncreases numbers, numIncreases . map sum $ slidingWindow 3 numbers)
