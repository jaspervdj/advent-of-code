{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified AdventOfCode.NanoParser as NP
import           Data.List.Extended      (group, lexicographicSuccessor)
import qualified System.IO               as IO

newtype Digit = Digit {unDigit :: Char} deriving (Enum, Eq, Ord, Show)

instance Bounded Digit where
    minBound = Digit '0'
    maxBound = Digit '9'

equalAdjacent :: Eq a => [a] -> Bool
equalAdjacent (x : y : zs) = if x == y then True else equalAdjacent (y : zs)
equalAdjacent _            = False

exactlyTwoEqualAdjacent :: Eq a => [a] -> Bool
exactlyTwoEqualAdjacent = (2 `elem`) . map length . group

neverDecrease :: Ord a => [a] -> Bool
neverDecrease (x : y : zs) = if y < x then False else neverDecrease (y : zs)
neverDecrease _            = True

parseRange :: NP.Parser Char ([Digit], [Digit])
parseRange = (,)
    <$> NP.many1 (Digit <$> NP.digit) <* NP.char '-'
    <*> NP.many1 (Digit <$> NP.digit)

main :: IO ()
main = do
    (lo, hi) <- NP.hRunParser IO.stdin parseRange
    print . length .
        takeWhile (<= hi) .
        filter (\pw -> neverDecrease pw && equalAdjacent pw) $
        iterate lexicographicSuccessor lo
    print . length .
        takeWhile (<= hi) .
        filter (\pw -> neverDecrease pw && exactlyTwoEqualAdjacent pw) $
        iterate lexicographicSuccessor lo
