module AdventOfCode.Ranges
    ( Ranges
    , empty
    , range

    , difference
    , intersection
    , union

    , size
    , minimum
    , maximum
    ) where

import           Data.List  (foldl')
import           Data.Maybe (catMaybes, maybeToList)
import           Prelude    hiding (maximum, minimum)
import qualified Prelude    as Prelude

data Range a = Range a a deriving (Eq, Show) -- Ordered, both inclusive.

newtype Ranges a = Ranges {unRanges :: [Range a]}

empty :: Ranges a
empty = Ranges []

mkRange :: Integral a => a -> a -> Maybe (Range a)
mkRange x y
    | x <= y    = Just $ Range x y
    | otherwise = Nothing

range :: Integral a => a -> a -> Ranges a
range x y = Ranges $ maybeToList $ mkRange x y

differenceRange :: Integral a => Range a -> Range a -> [Range a]
differenceRange (Range lo0 hi0) (Range lo1 hi1)
    -- Cut range is completely outside
    | hi0 < lo1  || lo0 > hi1  = [Range lo0 hi0]
    -- Cut range encompasses everything
    | lo0 >= lo1 && hi0 <= hi1 = []
    -- Cut range cuts left part
    | lo1 <= lo0               = maybeToList $ mkRange (hi1 + 1) hi0
    -- Cut range cuts right part
    | hi1 >= hi0               = maybeToList $ mkRange lo0 (lo1 - 1)
    -- Cuts in the middle
    | otherwise                = catMaybes
        [mkRange lo0 (lo1 - 1), mkRange (hi1 + 1) hi0]

difference :: Integral a => Ranges a -> Ranges a -> Ranges a
difference (Ranges ls) (Ranges rs) = Ranges $ do
    l <- ls
    r <- rs
    differenceRange l r

intersectRange :: Integral a => Range a -> Range a -> Maybe (Range a)
intersectRange (Range lo0 hi0) (Range lo1 hi1)
    | hi0 < lo1 = Nothing
    | lo0 > hi1 = Nothing
    | otherwise = Just $ Range (max lo0 lo1) (min hi0 hi1)

intersection :: Integral a => Ranges a -> Ranges a -> Ranges a
intersection (Ranges ls) (Ranges rs) = Ranges $ do
    l <- ls
    r <- rs
    maybeToList $ intersectRange l r

unionRange :: Integral a => Range a -> Range a -> Maybe (Range a)
unionRange (Range lo0 hi0) (Range lo1 hi1)
    | hi0 + 1 < lo1 || lo0 > hi1 + 1 = Nothing
    | otherwise                      = Just $ Range (min lo0 lo1) (max hi0 hi1)

insertRange :: Integral a => Range a -> Ranges a -> Ranges a
insertRange x = Ranges . go [] . unRanges
  where
    go acc [] = x : acc
    go acc (y : ys) = case unionRange x y of
        Nothing -> go (y : acc) ys
        Just z  -> unRanges . insertRange z . Ranges $ acc ++ ys

-- Warning: somewhat untested
union :: Integral a => Ranges a -> Ranges a -> Ranges a
union xs = foldl' (\ys x -> insertRange x ys) xs . unRanges

size :: Integral a => Ranges a -> a
size (Ranges rs) = sum [hi - lo + 1 | Range lo hi <- rs]

minimum, maximum :: Integral a => Ranges a -> a
minimum (Ranges rs) = Prelude.minimum [lo | Range lo _  <- rs]
maximum (Ranges rs) = Prelude.maximum [hi | Range _  hi <- rs]
