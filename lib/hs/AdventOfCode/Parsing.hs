-- | Dirty parsing module
module AdventOfCode.Parsing
    ( ints
    ) where

import           Data.Char  (isDigit)
import           Data.Maybe (mapMaybe)
import           Text.Read  (readMaybe)

-- | Extract all ints from a string.  Very dirty.
ints :: String -> [Int]
ints = mapMaybe readMaybe . words .
    map (\x -> if isDigit x || x == '-' then x else ' ')
