module AdventOfCode.BinarySearch
    ( upperBound
    ) where

-- | Return the last value for which the supplied function returns true.
upperBound :: (Int -> Bool) -> Int
upperBound f
    | not (f 0)   = error "AdventOfCode.BinarySearch.upperBound: false for 0"
    | otherwise   = findUpper 1
  where
    findUpper hi
        | f hi      = findUpper (hi * 2)
        | otherwise = bisect 0 hi

    bisect lo hi
        | lo + 1 >= hi = lo
        | f mid        = bisect mid hi
        | otherwise    = bisect lo mid
      where
        mid = (lo + hi) `div` 2
