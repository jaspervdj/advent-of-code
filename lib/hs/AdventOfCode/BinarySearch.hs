module AdventOfCode.BinarySearch
    ( upperBound
    , lowerBound
    ) where

import           Data.Maybe (isJust, isNothing)

-- | Return the last value for which the supplied function returns a value.
upperBound :: (Int -> Maybe a) -> Maybe a
upperBound f = case f 0 of
    Nothing -> Nothing
    _       -> findUpper 1
  where
    findUpper hi = case f hi of
        Just _ -> findUpper (hi * 2)
        _      -> bisect 0 hi

    bisect lo hi
        | lo + 1 >= hi   = f lo
        | isJust (f mid) = bisect mid hi
        | otherwise      = bisect lo mid
      where
        mid = (lo + hi) `div` 2

-- | Find the first value for which the supplied value returns a 'Just'.
lowerBound :: (Int -> Maybe a) -> Maybe a
lowerBound f = case f 0 of
    Just x  -> Just x
    Nothing -> findUpper 1
  where
    findUpper hi = case f hi of
        Nothing -> findUpper (hi * 2)
        Just _  -> bisect 0 hi

    bisect lo hi
        | lo + 1 >= hi      = f hi
        | isNothing (f mid) = bisect mid hi
        | otherwise         = bisect lo mid
      where
        mid = (lo + hi) `div` 2
