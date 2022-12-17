-- | Simple infinite list.
module AdventOfCode.Stream
    ( Stream (..)
    , fromListCycle

    , head
    , tail
    , uncons
    ) where

import Prelude hiding (head, tail)

data Stream a = Cons a (Stream a)

-- | Infinitely repeat the list.
fromListCycle :: [a] -> Stream a
fromListCycle l0 = go l0
  where
    go (x : xs) = Cons x (go xs)
    go []       = go l0

head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

uncons :: Stream a -> (a, Stream a)
uncons (Cons x xs) = (x, xs)
