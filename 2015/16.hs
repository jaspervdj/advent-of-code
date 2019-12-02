module Main where

import qualified AdventOfCode.NanoParser as NP
import qualified Data.Map                as Map
import qualified System.IO               as IO

type Analysis = Map.Map String Int

type Sue = (Int, Analysis)

parseSue :: NP.Parser Char Sue
parseSue = (,)
    <$> (NP.string "Sue" *> NP.spaces *> NP.decimal <* NP.char ':' <* NP.spaces)
    <*> (Map.fromList <$> NP.sepBy item (NP.char ',' <* NP.spaces))
  where
    item = (,)
        <$> (NP.many1 NP.alpha <* NP.char ':' <* NP.spaces)
        <*> (NP.decimal <* NP.spaces)

matchesExact :: Analysis -> Analysis -> Bool
matchesExact tape = and . Map.intersectionWith (==) tape

matchesRange :: Analysis -> Analysis -> Bool
matchesRange tape = and . Map.intersectionWithKey matches tape
  where
    matches "cats"      = (<)
    matches "trees"     = (<)
    matches "pomerians" = (>)
    matches "goldfish"  = (>)
    matches _           = (==)

tickerTape :: Analysis
tickerTape = Map.fromList
    [ ("children", 3)
    , ("cats", 7)
    , ("samoyeds", 2)
    , ("pomeranians", 3)
    , ("akitas", 0)
    , ("vizslas", 0)
    , ("goldfish", 5)
    , ("trees", 3)
    , ("cars", 2)
    , ("perfumes", 1)
    ]

main :: IO ()
main = do
    sues <- NP.hRunParser IO.stdin (NP.many1 parseSue)
    print . fst . head . filter (matchesExact tickerTape . snd) $ sues
    print . fst . head . filter (matchesRange tickerTape . snd) $ sues
