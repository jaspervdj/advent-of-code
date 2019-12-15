{-# LANGUAGE BangPatterns #-}
module Main where

import qualified AdventOfCode.AStar as AStar
import           Data.Char          (isAlpha, isSpace)
import qualified Data.List          as L
import           Data.Maybe         (maybeToList)
import           Data.Tuple         (swap)
import qualified System.IO          as IO

type Replacements a = [([a], [a])]

parseInput :: IO.Handle -> IO (Replacements Char, String)
parseInput h = do
    let keep c = isAlpha c || isSpace c
    ls <- filter (not . null) . lines . filter keep <$> IO.hGetContents h
    let replacements = [(x, y) | [x, y] <- map words (init ls)]
    pure (replacements, last ls)

-- Perform (all) single replacements
single :: Eq a => Replacements a -> [a] -> [[a]]
single _            []           = []
single replacements str@(x : xs) =
    [ replacement ++ suffix
    | (pattern, replacement) <- replacements
    , suffix <- maybeToList (L.stripPrefix pattern str)
    ] ++
    map (x :) (single replacements xs)

main :: IO ()
main = do
    (replacements, start) <- parseInput IO.stdin
    print $ length $ L.nub $ single replacements start
    let placements = map swap replacements
    print . maybe 0 fst $ AStar.astar
        (map ((,) 1) . single placements)
        (succ . length)
        start
        "e"
