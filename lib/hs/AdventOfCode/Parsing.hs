-- | Dirty parsing module
module AdventOfCode.Parsing
    ( ints
    , sections
    ) where

import           Data.Char  (isDigit)
import           Data.Maybe (mapMaybe)
import           Text.Read  (readMaybe)

-- | Extract all ints from a string.  Very dirty.
ints :: String -> [Int]
ints = mapMaybe readMaybe . words .
    map (\x -> if isDigit x || x == '-' then x else ' ')

-- | Extract all sections of non-empty lines separated by empty lines.
sections :: String -> [[String]]
sections = go . lines
  where
    go ls =
        let (section, trailing) = break null ls
            next = drop 1 trailing in
        (if not $ null section then [section] else []) ++
        (if null next then [] else go next)
